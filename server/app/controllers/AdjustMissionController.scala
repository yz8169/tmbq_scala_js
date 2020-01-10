package controllers

import java.io.File
import java.net.URLEncoder

import akka.actor.{Actor, ActorSystem, PoisonPill, Props}
import akka.stream.Materializer
import dao.AdjustMissionDao
import javax.inject.Inject
import org.apache.commons.io.FileUtils
import org.zeroturnaround.zip.ZipUtil
import play.api.libs.json.{JsValue, Json}
import play.api.libs.streams.ActorFlow
import play.api.mvc.{AbstractController, ControllerComponents, WebSocket}
import tool.Tool
import utils.Utils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps

/**
  * Created by yz on 2018/10/15
  */
class AdjustMissionController @Inject()(cc: ControllerComponents, formTool: FormTool, adjustMissionDao: AdjustMissionDao,
                                        tool: Tool)(implicit val system: ActorSystem,
                                                    implicit val materializer: Materializer) extends AbstractController(cc) {

  def deleteMissionById = Action.async {
    implicit request =>
      val data = formTool.missionIdForm.bindFromRequest().get
      adjustMissionDao.deleteById(data.missionId).map {
        x =>
          val missionIdDir = tool.getAdjustMissionIdDirById(data.missionId)
          Utils.deleteDirectory(missionIdDir)
          Redirect(routes.AdjustMissionController.getAllMission())
      }
  }

  def getAllMission = Action.async { implicit request =>
    val userId = tool.getUserId
    adjustMissionDao.selectAll(userId).map {
      x =>
        Future {
          val missionIds = x.map(_.id.toString)
          val missionDir = tool.getUserAdjustMissionDir
          missionDir.listFiles().filter { dir =>
            !missionIds.contains(dir.getName)
          }.foreach(Utils.deleteDirectory(_))
        }
        val array = Utils.getArrayByTs(x)
        Ok(Json.toJson(array))
    }
  }

  def getLogContent = Action.async {
    implicit request =>
      val userId = tool.getUserId
      val data = formTool.missionIdForm.bindFromRequest().get
      adjustMissionDao.selectByMissionId(userId, data.missionId).map {
        mission =>
          val missionIdDir = tool.getAdjustMissionIdDirById(data.missionId)
          val logFile = new File(missionIdDir, s"log.txt")
          val logStr = FileUtils.readFileToString(logFile, "UTF-8")
          Ok(Json.toJson(logStr))
      }
  }

  def updateMissionSocket = WebSocket.accept[JsValue, JsValue] {
    implicit request =>
      val userId = tool.getUserId
      var beforeMissions = Utils.execFuture(adjustMissionDao.selectAll(userId))
      var currentMissions = beforeMissions
      ActorFlow.actorRef(out => Props(new Actor {
        override def receive: Receive = {
          case msg: JsValue if (msg \ "info").as[String] == "start" =>
            out ! Utils.getJsonByTs(beforeMissions)
            system.scheduler.scheduleOnce(3 seconds, self, Json.obj("info" -> "update"))
          case msg: JsValue if (msg \ "info").as[String] == "update" =>
            adjustMissionDao.selectAll(userId).map {
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
      adjustMissionDao.selectByMissionId(userId, missionId).map {
        mission =>
          val missionIdDir = tool.getAdjustMissionIdDirById(missionId)
          val resultDir = new File(missionIdDir, "result")
          val resultFile = new File(missionIdDir, s"result.zip")
          if (!resultFile.exists()) ZipUtil.pack(resultDir, resultFile)
          Ok.sendFile(resultFile).withHeaders(
            CACHE_CONTROL -> "max-age=3600",
            CONTENT_DISPOSITION -> tool.getContentDisposition(s"${mission.missionName}.zip"),
            CONTENT_TYPE -> "application/x-download"
          )
      }
  }

  def missionNameCheck = Action.async { implicit request =>
    val data = formTool.missionNameForm.bindFromRequest.get
    val userId = tool.getUserId
    adjustMissionDao.selectOptionByMissionName(userId, data.missionName).map { mission =>
      mission match {
        case Some(y) => Ok(Json.obj("valid" -> false))
        case None =>
          Ok(Json.obj("valid" -> true))
      }
    }
  }

  def paramAdjustBefore() = Action { implicit request =>
    val data = formTool.missionIdOptionForm.bindFromRequest().get
    val missionName = s"param_${tool.generateMissionName}"
    Ok(views.html.user.paramAdjust(missionName, data.missionId))
  }

}
