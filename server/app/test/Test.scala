package test

import java.awt.Color
import java.io.{File, FileInputStream, FileOutputStream}
import java.math.BigInteger
import java.util.concurrent.ForkJoinPool

import org.apache.commons.io.FileUtils
import org.apache.commons.lang3.StringUtils
import org.apache.poi.hssf.usermodel.HSSFCellStyle
import org.apache.poi.ss.usermodel.{CellStyle, FillPatternType, IndexedColors}
import org.apache.poi.xssf.usermodel.{XSSFColor, XSSFWorkbook}
import org.zeroturnaround.zip.ZipUtil
import utils.Utils

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.immutable.ParRange
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.forkjoin.ForkJoinPool
import utils.Implicits._

import scala.collection.parallel.mutable.ParArray

/**
  * Created by yz on 2018/9/4
  */
object Test {

  def main(args: Array[String]): Unit = {

    val array = (1 to 10).toParArray
    array.zipWithIndex.threadNum(2).map { case (i, j) =>
      Thread.sleep(2000)
      println(s"${i}\t${Thread.currentThread().getName}")
    }

    ParRange
    ParArray



    //    val startTime = System.currentTimeMillis()
    //    val parent = new File("E:\\tmbq\\20181126_agilent test data1\\20181126_TOLR4 knock out serum test_3NPH")
    //    val file = new File(parent, "compound_config.xlsx")
    //    val lines = Utils.xlsx2Lines(file)
    //    val indexs = lines.drop(1).map { x =>
    //      x.split("\t")(9)
    //    }.filter(x => x != "none")
    //    val newLines = lines.map { line =>
    //      val columns = line.split("\t")
    //      columns(0) = if (indexs.contains(columns(0))) {
    //        s"is_${columns(0)}"
    //      } else columns(0)
    //      columns(9) = if (indexs.contains(columns(9))) {
    //        s"is_${columns(9)}"
    //      } else columns(9)
    //      columns(0)=if(Utils.isDouble(columns(10)) && !columns(0).startsWith("is")){
    //        s"is_${columns(0)}"
    //      }else columns(0)
    //      columns.mkString("\t")
    //    }
    //    Utils.lines2Xlsx(newLines, new File(parent, "out.xlsx"))


    //    val dtaDir = new File(parent.getParent, "out")
    //    val file = new File(parent, "18G19_C1.TXT")
    //    //500M
    //    val compoundFile = new File(parent.getParent, "compound_MRM_FUNCTION-3.xlsx")
    //    val compounds = getCompoundDatas(compoundFile)
    //    val functions = compounds.map(x => s"FUNCTION ${x.function}")
    //    val ftMap = getFtMap(file, functions)
    //    compounds.foreach { compound =>
    //      val function = s"FUNCTION ${compound.function}"
    //      val newLines = ArrayBuffer[String](s"#SEC\tMZ\tINT")
    //      val trueMz = ftMap(function).map { case ((mz, time), value) =>
    //        mz
    //      }.toBuffer.distinct.sortBy { x: Double => (compound.mz - x).abs }.head
    //      newLines ++= ftMap(function).filter { case ((mz, time), value) =>
    //        mz == trueMz
    //      }.map { case ((mz, time), values) =>
    //        //          println(compound, compound.index, values, file)
    //        s"${time * 60}\t${mz}\t${values(compound.index)}"
    //      }.toBuffer
    //      val dir = new File(dtaDir, compound.name)
    //      Utils.createDirectoryWhenNoExist(dir)
    //      val prefix = Utils.getPrefix(file)
    //      FileUtils.writeLines(new File(dir, s"${prefix}.dta"), newLines.asJava)
    //    }
    //    Thread.sleep(10000)
    //    println(Utils.getTime(startTime))

    //    getLines(new File(parent, "app"))
    //    getLines(new File(parent, "conf"))
    //    getLines(new File(parent, "rScripts"))
    //    getLines(new File(parent, "public/stylesheets"))
    //    getLines(new File(parent, "public/javascripts"))
    //    getLines(new File(parent, "public/bootstrap"))
    //    //    getLines(new File(parent, "public/select2-4.0.3"))
    //    getLines(new File(parent, "pyScripts"))
    //    getLines(new File(parent, "rScripts"))
    //    println(arrayBuffer.size)
    //    FileUtils.writeLines(new File("E:\\code.txt"), arrayBuffer.asJava)

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
    val map = mutable.Map[String, ArrayBuffer[String]]()
    var key = ""
    for (line <- lines) {
      if (line.startsWith("FUNCTION")) {
        key = line
        map += (key -> ArrayBuffer[String]())
      } else if (map.contains(key)) {
        map(key) += line
      }
    }
    val ftMap = mutable.Map[String, mutable.Map[(Double, Double), ArrayBuffer[Double]]]()
    map.withFilter(x => functions.contains(x._1)).foreach { case (key, lines) =>
      var timeKey = 0.0
      val map = mutable.Map[(Double, Double), ArrayBuffer[Double]]()
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

  val arrayBuffer = ArrayBuffer[String]()

  def getLines(file: File): ArrayBuffer[String] = {
    for (f <- file.listFiles()) {
      if (f.isDirectory) {
        getLines(f)
      } else {
        arrayBuffer ++= FileUtils.readLines(f).asScala
      }
    }
    arrayBuffer
  }


}
