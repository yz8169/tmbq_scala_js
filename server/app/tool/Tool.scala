package tool

import java.awt.Color
import java.io.{File, FileInputStream, FileOutputStream}
import java.net.URLEncoder
import java.nio.file.Files
import java.text.SimpleDateFormat
import java.util.concurrent.ForkJoinPool

import javax.inject.Inject
import org.joda.time.DateTime
import utils.Utils
import dao._
import org.apache.commons.io.FileUtils
import org.apache.commons.lang3.StringUtils
import org.apache.poi.ss.usermodel.{Cell, DateUtil, FillPatternType, IndexedColors}
import org.apache.poi.xssf.usermodel.{XSSFColor, XSSFWorkbook}
import play.api.mvc.RequestHeader
import utils.Pojo._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.forkjoin.ForkJoinPool
import utils.Implicits._

import scala.collection.parallel.mutable.ParArray


/**
  * Created by yz on 2018/9/19
  */
class Tool @Inject()(modeDao: ModeDao) {

  def generateMissionName = {
    (new DateTime).toString("yyyy_MM_dd_HH_mm_ss")
  }

  def isTestMode = {
    val mode = Utils.execFuture(modeDao.select)
    if (mode.test == "t") true else false
  }

  def createTempDirectory(prefix: String) = {
    if (isTestMode) Utils.testDir else Files.createTempDirectory(prefix).toFile
  }

  def getCompoundDatas(compoundConfigFile: File) = {
    val compoundLines = Utils.xlsx2Lines(compoundConfigFile)
    case class CompoundData(name: String, function: String, mz: Double, index: Int)
    val headers = compoundLines.head.split("\t").map(_.toLowerCase)
    val maps = compoundLines.drop(1).map { line =>
      val columns = line.split("\t")
      headers.zip(columns).toMap
    }
    val mzMap = maps.map { map =>
      val mzs = map("mass").split(">")
      val mz1 = if (mzs.size == 1) 0.0 else mzs(1).toDouble
      val function = map("function")
      ((function, mzs(0).toDouble), mz1)
    }.distinct.groupBy(_._1).mapValues(x => x.map(_._2).sorted)
    maps.map { map =>
      val name = map("compound")
      val function = map("function")
      val mzs = map("mass").split(">")
      val mz = mzs(0).toDouble
      val index = if (mzs.size == 1) {
        0
      } else {
        val mz1 = mzs(1).toDouble
        mzMap((function, mz)).indexOf(mz1)
      }
      CompoundData(name, function, mz, index)
    }
  }

  def getFtMap(file: File, functions: ArrayBuffer[String]) = {
    val lines = FileUtils.readLines(file).asScala
    val map = mutable.LinkedHashMap[String, ArrayBuffer[String]]()
    var key = ""
    for (line <- lines) {
      if (line.startsWith("FUNCTION")) {
        key = line
        map += (key -> ArrayBuffer[String]())
      } else if (map.contains(key)) {
        map(key) += line
      }
    }
    val ftMap = mutable.LinkedHashMap[String, mutable.Map[(Double, Double), ArrayBuffer[Double]]]()
    map.withFilter(x => functions.contains(x._1)).foreach { case (key, lines) =>
      var timeKey = 0.0
      val map = mutable.LinkedHashMap[(Double, Double), ArrayBuffer[Double]]()
      lines.filterNot(StringUtils.isBlank(_)).filterNot(_.startsWith("Scan")).foreach { line =>
        if (line.startsWith("Retention Time")) {
          timeKey = line.split("\t")(1).toDouble
        } else {
          val columns = line.split("\t")
          val t = (columns(0).toDouble, timeKey)
          if (map.isDefinedAt(t)) map(t) += columns(1).toDouble else map(t) = ArrayBuffer[Double](columns(1).toDouble)
        }
      }
      ftMap(key) = map
    }
    ftMap
  }

  def getAgilentFtMap(file: File) = {
    val lines = FileUtils.readLines(file).asScala
    lines.zipWithIndex.withFilter { case (line, i) =>
      line.trim.startsWith("index:")
    }.map { case (line, i) =>
      val trimLine = line.trim
      val r = "^index:\\s+(\\d+)$".r
      val r(index) = trimLine
      val buffer = lines.drop(i + 1).takeWhile { x =>
        !x.trim().startsWith("index:")
      }
      val times = buffer.zipWithIndex.withFilter { case (line, i) =>
        line.contains("cvParam: time array,")
      }.map(x => buffer(x._2 + 1)).flatMap { line =>
        line.trim.replaceAll("^.*\\]\\s+", "").split("\\s+").map(_.toDouble).toBuffer
      }
      val values = buffer.zipWithIndex.withFilter { case (line, i) =>
        line.contains("cvParam: intensity array,")
      }.map(x => buffer(x._2 + 1)).flatMap { line =>
        line.trim.replaceAll("^.*\\]\\s+", "").split("\\s+").map(_.toDouble).toBuffer
      }
      (index.toDouble, times.zip(values))
    }.toMap
  }

  def productDtaFiles(tmpDir: File, compoundConfigFile: File, dataDir: File, threadNum: Int) = {
    val dtaDir = new File(tmpDir, "dta")
    Utils.createDirectoryWhenNoExist(dtaDir)
    val compounds = getCompoundDatas(compoundConfigFile)
    val functions = compounds.map(x => s"FUNCTION ${x.function}")
    val files = dataDir.listFiles()
    val finalThreadNum = threadNum
    val map = files.zipWithIndex.map { case (v, i) =>
      val j = (i % finalThreadNum) + 1
      (j, v)
    }.groupBy(_._1).mapValues(_.map(_._2))
    val f = map.map { case (i, files) =>
      Future {
        files.foreach { file =>
          val ftMap = getFtMap(file, functions)
          compounds.foreach { compound =>
            val function = s"FUNCTION ${compound.function}"
            val newLines = ArrayBuffer[String](s"#SEC\tMZ\tINT")
            val trueMz = ftMap(function).map { case ((mz, time), value) =>
              mz
            }.toBuffer.distinct.sortBy { x: Double => (compound.mz - x).abs }.head
            newLines ++= ftMap(function).filter { case ((mz, time), value) =>
              mz == trueMz
            }.map { case ((mz, time), values) =>
              //          println(compound, compound.index, values, file)
              s"${time}\t${mz}\t${values(compound.index)}"
            }.toBuffer
            val dir = new File(dtaDir, compound.name)
            Utils.createDirectoryWhenNoExist(dir)
            val prefix = Utils.getPrefix(file)
            FileUtils.writeLines(new File(dir, s"${prefix}.dta"), newLines.asJava)
          }
        }
      }
    }.toBuffer.reduceLeft(_ zip _ map (x => ()))
    Utils.execFuture(f)
  }

  def productAgilentDtaFiles(tmpDir: File, compoundConfigFile: File, dataDir: File, threadNum: Int) = {
    val dtaDir = new File(tmpDir, "dta")
    Utils.createDirectoryWhenNoExist(dtaDir)
    val compounds = getCompoundDatas(compoundConfigFile)
    val files = dataDir.listFiles()
    val finalThreadNum = threadNum
    val map = files.zipWithIndex.map { case (v, i) =>
      val j = (i % finalThreadNum) + 1
      (j, v)
    }.groupBy(_._1).mapValues(_.map(_._2))
    val f = map.map { case (i, files) =>
      Future {
        files.foreach { file =>
          val ftMap = getAgilentFtMap(file)
          compounds.foreach { compound =>
            val function = s"${compound.function}"
            val newLines = ArrayBuffer[String](s"#SEC\tMZ\tINT")
            newLines ++= ftMap(function.toDouble).map { case (time, value) =>
              s"${time}\t${compound.mz}\t${value}"
            }
            val dir = new File(dtaDir, compound.name)
            Utils.createDirectoryWhenNoExist(dir)
            val prefix = Utils.getPrefix(file)
            FileUtils.writeLines(new File(dir, s"${prefix}.dta"), newLines.asJava)
          }
        }
      }
    }.toBuffer.reduceLeft(_ zip _ map (x => ()))
    Utils.execFuture(f)
  }

  def deleteDirectory(direcotry: File) = {
    if (!isTestMode) Utils.deleteDirectory(direcotry)
  }

  def getUserId(implicit request: RequestHeader) = {
    request.session.get("id").get.toInt
  }

  def getUserIdDir(implicit request: RequestHeader) = {
    val userId = getUserId
    new File(Utils.userDir, userId.toString)
  }

  def getUserIdDir(userId: Int) = {
    new File(Utils.userDir, userId.toString)
  }

  def getUserMissionDir(implicit request: RequestHeader) = {
    val userIdDir = getUserIdDir
    new File(userIdDir, "mission")
  }

  def getUserAdjustMissionDir(implicit request: RequestHeader) = {
    val userIdDir = getUserIdDir
    new File(userIdDir, "adjust_mission")
  }

  def getMissionIdDirById(missionId: Int)(implicit request: RequestHeader) = {
    val userMissionFile = getUserMissionDir
    new File(userMissionFile, missionId.toString)
  }

  def getAdjustMissionIdDirById(missionId: Int)(implicit request: RequestHeader) = {
    val userMissionFile = getUserAdjustMissionDir
    new File(userMissionFile, missionId.toString)
  }

  def getWorkspaceDirById(missionId: Int)(implicit request: RequestHeader) = {
    val missionIdDir = getMissionIdDirById(missionId)
    new File(missionIdDir, "workspace")
  }

  def getYellowStyle(workbook: XSSFWorkbook) = {
    val style = workbook.createCellStyle()
    style.setFillPattern(FillPatternType.SOLID_FOREGROUND)
    style.setFillForegroundColor(IndexedColors.YELLOW.getIndex)
    style
  }

  def getRedStyle(workbook: XSSFWorkbook) = {
    val style = workbook.createCellStyle()
    style.setFillPattern(FillPatternType.SOLID_FOREGROUND)
    style.setFillForegroundColor(IndexedColors.RED.getIndex)
    style
  }

  def getGreenStyle(workbook: XSSFWorkbook) = {
    val style = workbook.createCellStyle()
    style.setFillPattern(FillPatternType.SOLID_FOREGROUND)
    val color = new XSSFColor(new Color(146, 208, 80))
    style.setFillForegroundColor(color)
    style
  }

  def dye(file: File, colorFile: File, outFile: File) = {
    val outputWorkbook = new XSSFWorkbook()
    val outputSheet = outputWorkbook.createSheet("Sheet1")
    val format = outputWorkbook.createDataFormat()
    val lines = FileUtils.readLines(file).asScala
    val colorLines = FileUtils.readLines(colorFile).asScala.map(_.split("\t"))
    val yellowStyle = getYellowStyle(outputWorkbook)
    val redStyle = getRedStyle(outputWorkbook)
    val greenStyle = getGreenStyle(outputWorkbook)
    for (i <- 0 until lines.size) {
      val outputEachRow = outputSheet.createRow(i)
      val line = lines(i)
      val columns = line.split("\t")
      for (j <- 0 until columns.size) {
        val cell = outputEachRow.createCell(j)
        cell.setCellValue(columns(j))
        val bat = colorLines(i)(1)
        if (i > 0 && j > 1 && Utils.isDouble(bat)) {
          if (Utils.isDouble(columns(j))) {
            cell.setCellValue(columns(j).toDouble)
          }
          colorLines(i)(j) match {
            case "yellow" =>
              cell.setCellStyle(yellowStyle)
            case "red" => cell.setCellStyle(redStyle)
            case "green" => cell.setCellStyle(greenStyle)
            case _ =>
          }
        }

      }
    }
    val fileOutputStream = new FileOutputStream(outFile)
    outputWorkbook.write(fileOutputStream)
    fileOutputStream.close()
    outputWorkbook.close()
  }

  def getCompoundMap(comoundConfigFile: File) = {
    val lines = xlsx2Lines(comoundConfigFile)
    lines.drop(1).map { line =>
      getRowMap(lines(0), line)
    }
  }

  def getRowMap(header: String, line: String) = {
    val headers = header.split("\t")
    val columns = line.split("\t")
    headers.zip(columns).toMap
  }

  def xlsx2Lines(xlsxFile: File) = {
    val lines = Utils.xlsx2Lines(xlsxFile)
    ArrayBuffer(lines(0).toLowerCase) ++= lines.drop(1)
  }

  def productBaseRFile(tmpDir: File) = {
    val rBaseFile = new File(Utils.rPath, "base.R")
    FileUtils.copyFileToDirectory(rBaseFile, tmpDir)
  }

  val standardUnit = "mol/L"

  def getmwMap(mw: Double) = {
    val map = Map(
      (standardUnit, "g/mL") -> math.pow(10, -3) * mw,
      (standardUnit, "mg/mL") -> 1 * mw,
      (standardUnit, "mg/g") -> 1 * mw,
      (standardUnit, "ug/mL") -> math.pow(10, 3) * mw,
      (standardUnit, "ug/g") -> math.pow(10, 3) * mw,
      (standardUnit, "ug/uL") -> 1 * mw,
      (standardUnit, "ug/L") -> math.pow(10, 6) * mw,
      (standardUnit, "ng/g") -> math.pow(10, 6) * mw,
      (standardUnit, "ppm") -> math.pow(10, 3) * mw,
      (standardUnit, "ppb") -> math.pow(10, 6) * mw,
      (standardUnit, "ng/mL") -> math.pow(10, 6) * mw,
      (standardUnit, "ppt") -> math.pow(10, 9) * mw
    )
    map ++ map.map { case ((standardUnit, unit), v) =>
      (unit, standardUnit) -> 1 / v
    }
  }

  def getNoMwMap = {
    val map = Map(
      (standardUnit, "nM") -> math.pow(10, 9),
      (standardUnit, "uM") -> math.pow(10, 6),
      (standardUnit, "mM") -> math.pow(10, 3),
      (standardUnit, "nmol/g") -> math.pow(10, 6),
      (standardUnit, "nmol/mg") -> math.pow(10, 3),
      (standardUnit, "pmol/mg") -> math.pow(10, 6),
      (standardUnit, "umol/mg") -> math.pow(10, 0),
      (standardUnit, "umol/g") -> math.pow(10, 3)
    )
    map ++ map.map { case ((standardUnit, unit), v) =>
      (unit, standardUnit) -> 1 / v
    }
  }

  def urlEncode(url: String) = {
    URLEncoder.encode(url, "UTF-8")
  }

  def getContentDisposition(url: String) = {
    val encodeUrl = urlEncode(url)
    s"attachment; filename*=utf-8''${encodeUrl}"
  }

  case class MyMessage(valid: Boolean, message: String)

  def compoundFileCheck(file: File, sampleConfigFile: File): MyMessage = {
    val sampleHeaders = Utils.xlsx2Lines(sampleConfigFile).map(_.toLowerCase()).head.split("\t")
    val lines = Utils.xlsx2Lines(file).map(_.toLowerCase())
    val headers = lines.head.split("\t").map(_.toLowerCase)
    val repeatHeaders = headers.diff(headers.distinct)
    if (repeatHeaders.nonEmpty) {
      return MyMessage(false, s"物质信息配置文件表头 ${repeatHeaders.head} 重复!")
    }
    val hasHeaders = ArrayBuffer("index", "compound", "function", "mass", "rt", "rtlw", "rtrw", "peak_location",
      "response", "is_correction", "std", "polynomial_type", "origin", "ws4pp", "i4pp", "rs4rs", "mp4rs", "snr4pp",
      "lod", "loq", "nups4pp", "ndowns4pp", "ws4pa", "lp4e", "rp4e", "mp4e", "bline", "rmode", "rmis", "rmratio")
    val noExistHeaders = hasHeaders.diff(headers)
    if (noExistHeaders.nonEmpty) {
      return MyMessage(false, s"物质信息配置文件表头 ${noExistHeaders.head} 不存在!")
    }

    val repeatColumns = ArrayBuffer("index", "compound")
    val repeatMap = repeatColumns.map(x => (x, mutable.Set[String]())).toMap
    val factorMap = Map("peak_location" -> ArrayBuffer("nearest", "largest", "first", "all"),
      "response" -> ArrayBuffer("height", "area"),
      "polynomial_type" -> ArrayBuffer("linear", "quadratic"),
      "origin" -> ArrayBuffer("exclude", "include"),
      "bline" -> ArrayBuffer("yes", "no"),
      "rmode" -> ArrayBuffer("yes", "no"),

    )
    val indexs = lines.drop(1).map { line =>
      val columns = line.split("\t")
      val lineMap = headers.zip(columns).toMap
      lineMap("index")
    }.filter(_.startsWith("is"))
    lines.drop(1).zipWithIndex.foreach { case (line, i) =>
      val columns = line.split("\t").padTo(headers.size, "")
      val lineMap = headers.zip(columns).toMap
      if (columns.size > headers.size) {
        return MyMessage(false, s"物质信息配置文件第${i + 2}行列数不正确,存在多余列!")
      }
      columns.zipWithIndex.foreach { case (tmpColumn, j) =>
        val column = tmpColumn.toLowerCase()
        val header = headers(j)

        if (header == "mrt") {
          if (!Utils.isInt(column) || column.toInt < 0) {
            return MyMessage(false, s"物质信息配置文件第${i + 2}行第${j + 1}列必须为自然数!")
          }
        }

        if (StringUtils.isEmpty(column)) {
          return MyMessage(false, s"物质信息配置文件第${i + 2}行第${j + 1}列为空!")
        }
        if (repeatColumns.contains(header)) {
          if (repeatMap(header).contains(column)) {
            return MyMessage(false, s"物质信息配置文件第${i + 2}行第${j + 1}列重复!")
          } else repeatMap(header) += column
        }
        if (header == "compound") {
          val ILLEGAL_CHARACTERS = Array('/', '\n', '\r', '\t', '\0', '\f', '`', '?', '*', '\\', '<', '>', '|', '\"', ':')
          if (column.exists(ILLEGAL_CHARACTERS.contains(_))) {
            return MyMessage(false, s"物质信息配置文件第${i + 2}行第${j + 1}列出现特殊字符!")
          }
        }
        val intHeaders = ArrayBuffer("ws4pp", "i4pp", "mp4rs", "snr4pp", "nups4pp", "downs4pp", "lp4e", "rp4e", "mp4e",
          "function")
        if (intHeaders.contains(header)) {
          if (!Utils.isInt(column) || column.toInt < 0) {
            return MyMessage(false, s"物质信息配置文件第${i + 2}行第${j + 1}列必须为自然数!")
          }
        }
        if (header == "mass") {
          val values = column.split(">")
          if (!values.forall(x => Utils.isDouble(x))) {
            return MyMessage(false, s"物质信息配置文件第${i + 2}行第${j + 1}列必须为实数或以'>'分隔的两个实数!")
          }
        }
        val doubleColumns = ArrayBuffer("rt", "rtlw", "rtrw", "lod", "loq")
        if (doubleColumns.contains(header)) {
          if (!Utils.isDouble(column)) {
            return MyMessage(false, s"物质信息配置文件第${i + 2}行第${j + 1}列必须为实数!")
          }
        }
        if (factorMap.keySet.contains(header)) {
          if (!factorMap(header).contains(column)) {
            return MyMessage(false, s"物质信息配置文件第${i + 2}行第${j + 1}列只能为(${factorMap(header).mkString("、")})中的一个!")
          }
        }
        val oddColumns = ArrayBuffer("ws4pp", "ws4pa")
        if (oddColumns.contains(header)) {
          if (!Utils.isInt(column) || column.toInt < 0 || column.toInt % 2 == 0) {
            return MyMessage(false, s"物质信息配置文件第${i + 2}行第${j + 1}列必须为奇数!")
          }
        }
        if (header == "rmratio") {
          if (lineMap("rmode") == "yes" && !(column == "none" || Utils.isDouble(column))) {
            return MyMessage(false, s"物质信息配置文件第${i + 2}行第${j + 1}列必须为实数!")
          }
        }
        if (header == "is_correction") {
          if (!(column == "none" || indexs.contains(column))) {
            return MyMessage(false, s"物质信息配置文件第${i + 2}行第${j + 1}列必须为none或者某个存在的内标化合物的index列名称!")
          }
        }
        if (header == "rmis") {
          if (lineMap("rmode") == "yes" && !(indexs.contains(column))) {
            return MyMessage(false, s"物质信息配置文件第${i + 2}行第${j + 1}列必须为某个存在的内标化合物的index列名称!")
          }
        }
        if (header == "std") {
          if (!lineMap("index").startsWith("is") && !sampleHeaders.contains(column)) {
            return MyMessage(false, s"物质信息配置文件第${i + 2}行第${j + 1}列浓度信息在样品信息配置表中不存在!")
          }
          if ((lineMap("index").startsWith("is") && !Utils.isDouble(column))) {
            return MyMessage(false, s"物质信息配置文件第${i + 2}行第${j + 1}列，因为此样品为内标化合物，所以必须为实数!")
          }
        }
        if (header == "rs4rs") {
          if (!Utils.isDouble(column) || column.toDouble > 1 || column.toDouble < 0) {
            return MyMessage(false, s"物质信息配置文件第${i + 2}行第${j + 1}列必须为0-1之间的实数!")
          }
        }
      }
      if (lineMap("mp4rs").toDouble > lineMap("mp4e").toDouble) {
        return MyMessage(false, s"物质信息配置文件第${i + 2}行mp4e必须大于等于mp4rs!")
      }

    }
    MyMessage(true, "")
  }

  def sampleFileCheck(file: File, fileNames: Seq[String]): MyMessage = {
    val lines = Utils.xlsx2Lines(file).map(_.toLowerCase())
    val headers = lines.head.split("\t").map(_.toLowerCase)
    if (headers.size < 4) {
      return MyMessage(false, s"样品信息配置文件列数必须大于等于4!")
    }
    val repeatHeaders = headers.diff(headers.distinct)
    if (repeatHeaders.nonEmpty) {
      return MyMessage(false, s"样品信息配置文件表头 ${repeatHeaders.head} 重复!")
    }
    val hasHeaders = ArrayBuffer("batch", "file name", "sample type")
    val noExistHeaders = hasHeaders.diff(headers)
    if (noExistHeaders.nonEmpty) {
      return MyMessage(false, s"样品信息配置文件表头 ${noExistHeaders.head} 不存在!")
    }
    val repeatColumns = ArrayBuffer("file name")
    val repeatMap = repeatColumns.map(x => (x, mutable.Set[String]())).toMap
    val factorMap = Map(
      "sample type" -> ArrayBuffer("standard", "analyte")
    )
    case class BatchData(batch: String, kind: String)
    val batchs = lines.drop(1).map { line =>
      val columns = line.split("\t")
      val lineMap = headers.zip(columns).toMap
      BatchData(lineMap("batch"), lineMap("sample type"))
    }.groupBy(_.batch)
    batchs.foreach { case (batch, batchDatas) =>
      val standrads = batchDatas.filter(_.kind == "standard")
      val analytes = batchDatas.filter(_.kind == "analyte")
      if (standrads.size < 2) {
        return MyMessage(false, s"样品信息配置文件batch${batch}标样不够(至少两个)!")
      }
      if (analytes.size < 1) {
        return MyMessage(false, s"样品信息配置文件batch${batch}待测样不够(至少1个)!")
      }
    }

    lines.drop(1).zipWithIndex.foreach { case (line, i) =>
      val columns = line.split("\t")
      if (columns.size > headers.size) {
        return MyMessage(false, s"样品信息配置文件第${i + 2}行列数不正确,存在多余列!")
      }
      val lineMap = headers.zip(columns).toMap
      columns.zipWithIndex.foreach { case (tmpColumn, j) =>
        val column = tmpColumn.toLowerCase()
        val header = headers(j)
        if (hasHeaders.contains(header)) {
          if (StringUtils.isEmpty(column)) {
            return MyMessage(false, s"样品信息配置文件第${i + 2}行第${j + 1}列为空!")
          }
        }
        if (repeatColumns.contains(header)) {
          if (repeatMap(header).contains(column)) {
            return MyMessage(false, s"样品信息配置文件第${i + 2}行第${j + 1}列重复!")
          } else repeatMap(header) += column
        }
        if (factorMap.keySet.contains(header)) {
          if (!factorMap(header).contains(column)) {
            return MyMessage(false, s"样品信息配置文件第${i + 2}行第${j + 1}列只能为(${factorMap(header).mkString("、")})中的一个!")
          }
        }
        if (header == "file name") {
          if (!fileNames.contains(column)) {
            return MyMessage(false, s"样品信息配置文件第${i + 2}行第${j + 1}列文件名不存在!")
          }
        }
      }
    }
    MyMessage(true, "")
  }


}

object Tool {

  def isFindPeak(tmpDir: File, isIndexs: Seq[IndexData], threadNum: Int) = {
    val isIndexPar = isIndexs.par.zipWithIndex
    isIndexPar.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(threadNum))
    val commandsPar = isIndexPar.map { case (indexData, i) =>
      val fileName = s"is_${i}"
      val isDir = new File(tmpDir, fileName)
      Utils.createDirectoryWhenNoExist(isDir)
      val compoundNameFile = new File(isDir, "compoundName.xlsx")
      val newLines = ArrayBuffer("CompoundName", indexData.compoundName)
      Utils.lines2Xlsx(newLines, compoundNameFile)
      val command =
        s"""
           |Rscript ${new File(Utils.rPath, "isFindPeak.R").toUnixPath} --ci ${compoundNameFile.getName} --co color.txt --io intensity.txt
           """.stripMargin
      CommandData(isDir, ArrayBuffer(command))
    }
    commandsPar.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(threadNum))
    commandsPar

  }

  def cFindPeak(tmpDir: File, indexDatas: Seq[IndexData], threadNum: Int) = {
    val cIndexs = indexDatas.filter(x => !x.index.startWithsIgnoreCase("is"))
    val cIndexPar = cIndexs.par.zipWithIndex
    cIndexPar.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(threadNum))
    val commandsPar = cIndexPar.map { case (indexData, i) =>
      val fileName = s"c_${i}"
      val cDir = new File(tmpDir, fileName)
      Utils.createDirectoryWhenNoExist(cDir)
      val compoundNameFile = new File(cDir, "compoundName.xlsx")
      val newLines = ArrayBuffer("CompoundName", indexData.compoundName)
      Utils.lines2Xlsx(newLines, compoundNameFile)
      val command =
        s"""
           |Rscript ${new File(Utils.rPath, "c_findPeak.R").toUnixPath} --ci ${compoundNameFile.getName} --co color.txt --io intensity.txt
           """.stripMargin
      CommandData(cDir, ArrayBuffer(command))
    }
    commandsPar.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(threadNum))
    commandsPar

  }

  def eachRegress(tmpDir: File, threadNum: Int) = {
    val dirs = tmpDir.listFiles().filter(_.isDirectory).
      filter(x => x.getName.startsWith("is_") || x.getName.startsWith("c_"))
    val commandsPar = dirs.map { dir =>
      val command =
        s"""
           |Rscript ${new File(Utils.rPath, "each_regress.R").toUnixPath} --ci compoundName.xlsx  --coi color.txt --ro regress.txt
                          """.stripMargin
      CommandData(dir, ArrayBuffer(command))
    }.par
    commandsPar.threadNum(threadNum)

  }


  def intensityMerge(tmpDir: File) = {
    val command =
      s"""
         |Rscript ${Utils.dosPath2Unix(new File(Utils.rPath, "intensity_merge.R"))}
           """.stripMargin
    CommandData(tmpDir, ArrayBuffer(command))

  }

  def allMerge(tmpDir: File) = {
    val command =
      s"""
         |Rscript ${Utils.dosPath2Unix(new File(Utils.rPath, "all_merge.R"))}
           """.stripMargin
    CommandData(tmpDir, ArrayBuffer(command))

  }

  def isMerge(tmpDir: File, isIndexs: Seq[IndexData]) = {
    val command = if (isIndexs.isEmpty) {
      ""
    } else {
      s"""
         |Rscript ${Utils.dosPath2Unix(new File(Utils.rPath, "is_rt_merge.R"))}
           """.stripMargin
    }
    CommandData(tmpDir, ArrayBuffer(command))

  }


}
