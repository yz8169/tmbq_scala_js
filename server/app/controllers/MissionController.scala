package controllers

import java.io.File
import java.net.URLEncoder

import akka.actor.{Actor, ActorSystem, PoisonPill, Props}
import akka.stream.Materializer
import command.{CommandExec}
import dao._
import javax.inject.Inject
import org.apache.commons.io.FileUtils
import org.apache.commons.lang3.StringUtils
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{AbstractController, ControllerComponents, WebSocket}
import tool.Tool
import utils.Utils

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import models.Tables._
import org.joda.time.DateTime
import org.zeroturnaround.zip.ZipUtil
import play.api.libs.streams.ActorFlow

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.collection.parallel.ForkJoinTaskSupport
import implicits.Implicits._
import mission.MissionUtils
import tool.Pojo.{CommandData, IndexData}
import scala.language.postfixOps


/**
 * Created by yz on 2018/9/18
 */
class MissionController @Inject()(cc: ControllerComponents, formTool: FormTool, userDao: UserDao,
                                  accountDao: AccountDao, tool: Tool,
                                  missionDao: MissionDao, adjustMissionDao: AdjustMissionDao)(implicit val system: ActorSystem,
                                                                                              implicit val materializer: Materializer) extends AbstractController(cc) {

  def newMissionBefore = Action { implicit request =>
    val missionName = s"project_${tool.generateMissionName}"
    Ok(views.html.user.newMission(missionName))

  }

  def newMission = Action.async(parse.multipartFormData) { implicit request =>
    val data = formTool.missionForm.bindFromRequest().get
    val userId = tool.getUserId
    val row = MissionRow(0, s"${data.missionName}", userId, new DateTime(), None, "running")
    missionDao.insert(row).flatMap(_ => missionDao.selectByMissionName(row.userId, row.missionName)).map { mission =>
      val outDir = tool.getUserMissionDir
      val missionDir = MissionUtils.getMissionDir(mission.id, outDir)
      val (tmpDir, resultDir) = (missionDir.workspaceDir, missionDir.resultDir)
      val dataDir = new File(tmpDir, "data")
      val tmpDataDir = new File(tmpDir, "tmpData")
      Utils.createDirectoryWhenNoExist(dataDir)
      val file = new File(tmpDir, "data.zip")
      val dataZipFile = request.body.file("dataFiles").get
      dataZipFile.ref.moveTo(file, replace = true)
      val sampleConfigExcelFile = new File(tmpDir, "sample_config.xlsx")
      val sampleConfigTempFile = request.body.file("sampleConfigFile").get
      sampleConfigTempFile.ref.moveTo(sampleConfigExcelFile, replace = true)
      Utils.removeXlsxBlankLine(sampleConfigExcelFile)
      val compoundConfigFile = new File(tmpDir, "compound_config.xlsx")
      val compoundConfigTempFile = request.body.file("compoundConfigFile").get
      compoundConfigTempFile.ref.moveTo(compoundConfigFile, replace = true)
      Utils.removeXlsxBlankLine(compoundConfigFile)
      val f = Future {
        ZipUtil.unpack(file, tmpDataDir)
        Utils.getAllFiles(tmpDataDir).foreach { file =>
          val destFile = new File(dataDir, file.getName.toLowerCase)
          FileUtils.copyFile(file, destFile)
        }
        tool.productDtaFiles(tmpDir, compoundConfigFile, dataDir, data.threadNum)
        val rBaseFile = new File(Utils.rPath, "base.R")
        FileUtils.copyFileToDirectory(rBaseFile, tmpDir)
      }

      f.flatMap { x =>
        val compoundLines = compoundConfigFile.xlsxLines()
        val threadNum = data.threadNum
        val indexDatas = compoundLines.lineMap.map { map =>
          IndexData(map("index"), map("compound"))
        }
        val isIndexs = indexDatas.filter(x => x.index.startWithsIgnoreCase("is"))

        val logFile = new File(tmpDir.getParent, "log.txt")
        val commandExecutor = CommandExec().parExec { b =>
          //is find peak
          Tool.isFindPeak(tmpDir, isIndexs, data.threadNum)
        }.exec { b =>
          //is merge
          Tool.isMerge(tmpDir, isIndexs)
        }.parExec { b =>
          //compound find peak
          Tool.cFindPeak(tmpDir, indexDatas, data.threadNum)
        }.exec { b =>
          //intensity merge
          Tool.intensityMerge(tmpDir)
        }.parExec { b =>
          //regress
          Tool.eachRegress(tmpDir, threadNum)
        }.exec { b =>
          //all merge
          Tool.allMerge(tmpDir)
        }
        val state = if (commandExecutor.isSuccess) {
          val intensityTxtFile = new File(tmpDir, "intensity.txt")
          val intensityExcelFile = new File(resultDir, "intensity.xlsx")
          Utils.txt2Xlsx(intensityTxtFile, intensityExcelFile)
          val regressTxtFile = new File(tmpDir, "regress.txt")
          val regressColorFile = new File(tmpDir, "color.txt")
          val regressExcelFile = new File(resultDir, "concentration.xlsx")
          tool.dye(regressTxtFile, regressColorFile, regressExcelFile)
          if (data.isPlot) {
            FileUtils.copyDirectoryToDirectory(new File(tmpDir, "plot_peaks"), resultDir)
            FileUtils.copyDirectoryToDirectory(new File(tmpDir, "plot_regress"), resultDir)
          }
          val originalDataDir = new File(resultDir.getParent, "data")
          Utils.createDirectoryWhenNoExist(originalDataDir)
          FileUtils.copyFileToDirectory(sampleConfigExcelFile, originalDataDir)
          FileUtils.copyFileToDirectory(compoundConfigFile, originalDataDir)
          FileUtils.copyFileToDirectory(file, originalDataDir)
          "success"
        } else {
          "error"
        }
        val newMission = mission.copy(state = state, endTime = Some(new DateTime()))
        missionDao.update(newMission)
      }.onComplete {
        case Failure(exception) =>
          exception.printStackTrace()
          FileUtils.writeStringToFile(missionDir.logFile, exception.toString)
          val newMission = mission.copy(state = "error", endTime = Some(new DateTime()))
          missionDao.update(newMission)
        case Success(x) =>
      }
      Ok(Json.obj("valid" -> true))
    }

  }

  def newAgilentMission = Action.async(parse.multipartFormData) { implicit request =>
    val data = formTool.missionForm.bindFromRequest().get
    val userId = tool.getUserId
    val row = MissionRow(0, s"${data.missionName}", userId, new DateTime(), None, "running")
    missionDao.insert(row).flatMap(_ => missionDao.selectByMissionName(row.userId, row.missionName)).map { mission =>
      val outDir = tool.getUserMissionDir
      val missionDir = MissionUtils.getMissionDir(mission.id, outDir)
      val (tmpDir, resultDir) = (missionDir.workspaceDir, missionDir.resultDir)
      val dataDir = new File(tmpDir, "data")
      val tmpDataDir = new File(tmpDir, "tmpData")
      Utils.createDirectoryWhenNoExist(dataDir)
      val file = new File(tmpDir, "data.zip")
      val dataZipFile = request.body.file("dataFiles").get
      dataZipFile.ref.moveTo(file, replace = true)
      val sampleConfigExcelFile = new File(tmpDir, "sample_config.xlsx")
      val sampleConfigTempFile = request.body.file("sampleConfigFile").get
      sampleConfigTempFile.ref.moveTo(sampleConfigExcelFile, replace = true)
      Utils.removeXlsxBlankLine(sampleConfigExcelFile)
      val compoundConfigFile = new File(tmpDir, "compound_config.xlsx")
      val compoundConfigTempFile = request.body.file("compoundConfigFile").get
      compoundConfigTempFile.ref.moveTo(compoundConfigFile, replace = true)
      Utils.removeXlsxBlankLine(compoundConfigFile)
      val f = Future {
        ZipUtil.unpack(file, tmpDataDir)
        Utils.getAllFiles(tmpDataDir).foreach { file =>
          val destFile = new File(dataDir, file.getName.toLowerCase)
          FileUtils.copyFile(file, destFile)
        }
        tool.productAgilentDtaFiles(tmpDir, compoundConfigFile, dataDir, data.threadNum)
        val rBaseFile = new File(Utils.rPath, "base.R")
        FileUtils.copyFileToDirectory(rBaseFile, tmpDir)
      }

      f.flatMap { x =>
        val compoundLines = compoundConfigFile.xlsxLines()
        val threadNum = data.threadNum
        val indexDatas = compoundLines.lineMap.map { map =>
          IndexData(map("index"), map("compound"))
        }
        val isIndexs = indexDatas.filter(x => x.index.startWithsIgnoreCase("is"))

        val logFile = new File(tmpDir.getParent, "log.txt")
        val commandExecutor = CommandExec().parExec { b =>
          //is find peak
          Tool.isFindPeak(tmpDir, isIndexs, data.threadNum)
        }.exec { b =>
          //is merge
          Tool.isMerge(tmpDir, isIndexs)
        }.parExec { b =>
          //compound find peak
          Tool.cFindPeak(tmpDir, indexDatas, data.threadNum)
        }.exec { b =>
          //intensity merge
          Tool.intensityMerge(tmpDir)
        }.parExec { b =>
          //regress
          Tool.eachRegress(tmpDir, threadNum)
        }.exec { b =>
          //all merge
          Tool.allMerge(tmpDir)
        }
        val state = if (commandExecutor.isSuccess) {
          val intensityTxtFile = new File(tmpDir, "intensity.txt")
          val intensityExcelFile = new File(resultDir, "intensity.xlsx")
          Utils.txt2Xlsx(intensityTxtFile, intensityExcelFile)
          val regressTxtFile = new File(tmpDir, "regress.txt")
          val regressColorFile = new File(tmpDir, "color.txt")
          val regressExcelFile = new File(resultDir, "concentration.xlsx")
          tool.dye(regressTxtFile, regressColorFile, regressExcelFile)
          if (data.isPlot) {
            FileUtils.copyDirectoryToDirectory(new File(tmpDir, "plot_peaks"), resultDir)
            FileUtils.copyDirectoryToDirectory(new File(tmpDir, "plot_regress"), resultDir)
          }
          val originalDataDir = new File(resultDir.getParent, "data")
          Utils.createDirectoryWhenNoExist(originalDataDir)
          FileUtils.copyFileToDirectory(sampleConfigExcelFile, originalDataDir)
          FileUtils.copyFileToDirectory(compoundConfigFile, originalDataDir)
          FileUtils.copyFileToDirectory(file, originalDataDir)
          "success"
        } else {
          "error"
        }
        val newMission = mission.copy(state = state, endTime = Some(new DateTime()))
        missionDao.update(newMission)
      }.onComplete {
        case Failure(exception) =>
          exception.printStackTrace()
          FileUtils.writeStringToFile(missionDir.logFile, exception.toString)
          val newMission = mission.copy(state = "error", endTime = Some(new DateTime()))
          missionDao.update(newMission)
        case Success(x) =>
      }
      Ok(Json.obj("valid" -> true))
    }

  }

  def newAgilentMissionBefore = Action { implicit request =>
    val missionName = s"project_${tool.generateMissionName}"
    Ok(views.html.user.newAgilentMission(missionName))

  }

  def fileCheck = Action(parse.multipartFormData) { implicit request =>
    val data = formTool.fileNamesForm.bindFromRequest().get
    val fileNames = data.fileNames.filter(StringUtils.isNotBlank(_)).map(Utils.getPrefix(_)).map(_.toLowerCase())
    val tmpDir = tool.createTempDirectory("tmpDir")
    val compoundConfigFile = new File(tmpDir, "compound_config.xlsx")
    val compoundConfigTempFile = request.body.file("compoundConfigFile").get
    compoundConfigTempFile.ref.moveTo(compoundConfigFile, replace = true)
    val sampleConfigFile = new File(tmpDir, "sample_config.xlsx")
    val sampleConfigTempFile = request.body.file("sampleConfigFile").get
    sampleConfigTempFile.ref.moveTo(sampleConfigFile, replace = true)
    var myMessage = tool.compoundFileCheck(compoundConfigFile, sampleConfigFile)
    if (myMessage.valid) {
      myMessage = tool.sampleFileCheck(sampleConfigFile, fileNames)
    }
    tool.deleteDirectory(tmpDir)
    Ok(Json.obj("valid" -> myMessage.valid, "message" -> myMessage.message))
  }

  def getAllMission = Action.async { implicit request =>
    val userId = tool.getUserId
    missionDao.selectAll(userId).map {
      x =>
        Future {
          val missionIds = x.map(_.id.toString)
          val missionDir = tool.getUserMissionDir
          missionDir.listFiles().filter { dir =>
            !missionIds.contains(dir.getName)
          }.foreach(Utils.deleteDirectory(_))
        }
        val array = Utils.getArrayByTs(x)
        Ok(Json.toJson(array))
    }
  }

  def deleteMissionById = Action.async {
    implicit request =>
      val data = formTool.missionIdForm.bindFromRequest().get
      missionDao.deleteById(data.missionId).map {
        x =>
          val missionIdDir = tool.getMissionIdDirById(data.missionId)
          Utils.deleteDirectory(missionIdDir)
          Redirect(routes.MissionController.getAllMission())
      }
  }

  def getLogContent = Action.async {
    implicit request =>
      val userId = tool.getUserId
      val data = formTool.missionIdForm.bindFromRequest().get
      missionDao.selectByMissionId(userId, data.missionId).map {
        mission =>
          val missionIdDir = tool.getMissionIdDirById(data.missionId)
          val logFile = new File(missionIdDir, s"log.txt")
          val logStr = FileUtils.readFileToString(logFile, "UTF-8")
          Ok(Json.toJson(logStr))
      }
  }

  def updateMissionSocket = WebSocket.accept[JsValue, JsValue] {
    implicit request =>
      val userId = tool.getUserId
      var beforeMissions = Utils.execFuture(missionDao.selectAll(userId))
      var currentMissions = beforeMissions
      ActorFlow.actorRef(out => Props(new Actor {
        override def receive: Receive = {
          case msg: JsValue if (msg \ "info").as[String] == "start" =>
            out ! Utils.getJsonByTs(beforeMissions)
            system.scheduler.scheduleOnce(3 seconds, self, Json.obj("info" -> "update"))
          case msg: JsValue if (msg \ "info").as[String] == "update" =>
            missionDao.selectAll(userId).map {
              missions =>
                currentMissions = missions
                if (currentMissions.size != beforeMissions.size) {
                  out ! Utils.getJsonByTs(currentMissions)
                } else {
                  val b = currentMissions.zip(beforeMissions).forall {
                    case (currentMission, beforeMission) =>
                      currentMission.id == beforeMission.id && currentMission.state == beforeMission.state
                  }
                  if (!b) {
                    out ! Utils.getJsonByTs(currentMissions)
                  }
                }
                beforeMissions = currentMissions
                system.scheduler.scheduleOnce(3 seconds, self, Json.obj("info" -> "update"))
            }
          case _ =>
            self ! PoisonPill
        }

        override def postStop(): Unit = {
          self ! PoisonPill
        }

      }))

  }

  def downloadResult = Action.async {
    implicit request =>
      val userId = tool.getUserId
      val data = formTool.missionIdForm.bindFromRequest().get
      val missionId = data.missionId
      missionDao.selectByMissionId(userId, missionId).map {
        mission =>
          val missionIdDir = tool.getMissionIdDirById(missionId)
          val resultDir = new File(missionIdDir, "result")
          val resultFile = new File(missionIdDir, s"result.zip")
          if (!resultFile.exists()) ZipUtil.pack(resultDir, resultFile)
          Ok.sendFile(resultFile).withHeaders(
            CACHE_CONTROL -> "max-age=3600",
            CONTENT_DISPOSITION -> tool.getContentDisposition(s"${mission.missionName}_result.zip"),
            CONTENT_TYPE -> "application/x-download"
          )
      }
  }

  def downloadData = Action.async {
    implicit request =>
      val userId = tool.getUserId
      val data = formTool.missionIdForm.bindFromRequest().get
      val missionId = data.missionId
      missionDao.selectByMissionId(userId, missionId).map {
        mission =>
          val missionIdDir = tool.getMissionIdDirById(missionId)
          val dataDir = new File(missionIdDir, "data")
          val dataFile = new File(missionIdDir, s"data.zip")
          if (!dataFile.exists()) ZipUtil.pack(dataDir, dataFile)
          Ok.sendFile(dataFile).withHeaders(
            CACHE_CONTROL -> "max-age=3600",
            CONTENT_DISPOSITION -> tool.getContentDisposition(s"${mission.missionName}_data.zip"),
            CONTENT_TYPE -> "application/x-download"
          )
      }
  }

  def missionNameCheck = Action.async { implicit request =>
    val data = formTool.missionNameForm.bindFromRequest.get
    val userId = tool.getUserId
    missionDao.selectOptionByMissionName(userId, data.missionName).map { mission =>
      mission match {
        case Some(y) => Ok(Json.obj("valid" -> false))
        case None =>
          Ok(Json.obj("valid" -> true))
      }
    }
  }

  def getAllSuccessMissionNames = Action.async { implicit request =>
    val userId = tool.getUserId
    missionDao.selectAll(userId, "success").map {
      x =>
        val array = x.map { mission =>
          Json.obj("text" -> mission.missionName, "id" -> mission.id)
        }
        Ok(Json.toJson(array))
    }
  }

  def getAllCompounds = Action { implicit request =>
    val data = formTool.missionIdForm.bindFromRequest().get
    val workspaceDir = tool.getWorkspaceDirById(data.missionId)
    val comoundConfigFile = new File(workspaceDir, "compound_config.xlsx")
    val compoundMap = tool.getCompoundMap(comoundConfigFile)
    val compoundNames = compoundMap.map { map =>
      map("compound")
    }
    Ok(Json.toJson(compoundNames))
  }

  def getArgs = Action { implicit request =>
    val data = formTool.argsForm.bindFromRequest().get
    val workspaceDir = tool.getWorkspaceDirById(data.missionId)
    val comoundConfigFile = new File(workspaceDir, "compound_config.xlsx")
    val compoundMap = tool.getCompoundMap(comoundConfigFile)
    val compoundRow = compoundMap.find { map =>
      map("compound") == data.compoundName
    }.get
    val fl = Json.obj("fl" -> compoundRow("ws4pp"), "snr" -> compoundRow("snr4pp"),
      "nups" -> compoundRow("nups4pp"), "ndowns" -> compoundRow("ndowns4pp"), "iteration" -> compoundRow("i4pp"),
      "bLine" -> compoundRow("bline"), "rtlw" -> compoundRow("rtlw"), "rtrw" -> compoundRow("rtrw"),
      "rt" -> compoundRow("rt"), "peakLocation" -> compoundRow("peak_location").toLowerCase
    )
    Ok(fl)
  }

  def paramAdjust = Action.async { implicit request =>
    val tmpDir = tool.createTempDirectory("tmpDir")
    val data = formTool.paramAdjustForm.bindFromRequest().get
    val userId = tool.getUserId
    val mission = Utils.execFuture(missionDao.selectByMissionId(userId, data.missionId))
    val args = ArrayBuffer(
      s"WS4PP:${data.flMin} to ${data.flMax} by ${data.step}",
      s"I4PP:${data.iteration}",
      s"NUPS4PP:${data.nups}",
      s"NDOWNS4PP:${data.ndowns}",
      s"SNR4PP:${data.snr}",
      s"BLine:${data.bLine}",
      s"RT:${data.rt}",
      s"RTLW:${data.rtlw}",
      s"RTRW:${data.rtrw}",
      s"Peak location:${data.peakLocation}",
    )
    val argStr = args.mkString(";")
    val row = AdjustMissionRow(0, mission.missionName, data.compoundName, argStr, s"${data.missionName}", userId, new DateTime(), None, "running")
    adjustMissionDao.insert(row).flatMap(_ => adjustMissionDao.selectByMissionName(row.userId, row.missionName)).map { mission =>
      val outDir = tool.getUserAdjustMissionDir
      val missionDir = MissionUtils.getMissionDir(mission.id, outDir)
      val (tmpDir, resultDir) = (missionDir.workspaceDir, missionDir.resultDir)
      val workspaceDir = tool.getWorkspaceDirById(data.missionId)
      val f = Future {
        FileUtils.copyFileToDirectory(new File(workspaceDir, "sample_config.xlsx"), tmpDir)
        FileUtils.copyFileToDirectory(new File(workspaceDir, "compound_config.xlsx"), tmpDir)
        FileUtils.copyDirectoryToDirectory(new File(workspaceDir, "dta"), tmpDir)
        val argsFile = new File(tmpDir, "args.txt")
        val lines = ArrayBuffer(ArrayBuffer("compound", "flMin", "flMax", "step", "nups", "ndowns", "snr", "iteration", "bline", "rtlw", "rtrw", "rt", "peakLocation").mkString("\t"))
        lines += (ArrayBuffer(data.compoundName, data.flMin, data.flMax, data.step, data.nups, data.ndowns, data.snr,
          data.iteration, data.bLine, data.rtlw, data.rtrw, data.rt, data.peakLocation).mkString("\t"))
        FileUtils.writeLines(argsFile, lines.asJava)
        tool.productBaseRFile(tmpDir)
      }

      val command =
        s"""
           |Rscript ${new File(Utils.rPath, "argsAdjust.R").unixPath}
       """.stripMargin
      f.map { _ =>
        val commandExec = CommandExec().exec { b =>
          CommandData(tmpDir, List(command))
        }.map { b =>
          FileUtils.copyDirectoryToDirectory(new File(tmpDir, "plot_peaks"), resultDir)
        }
        if (commandExec.isSuccess) "success" else "error"
      }.map { state =>
        val newMission = mission.copy(state = state, endTime = Some(new DateTime()))
        adjustMissionDao.update(newMission)
      }

      Ok(Json.obj("valid" -> true))
    }
  }

  def paramAdjustResultBefore = Action { implicit request =>
    Ok(views.html.user.adjustMissionManage())
  }


}
