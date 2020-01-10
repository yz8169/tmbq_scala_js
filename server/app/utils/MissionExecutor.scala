package utils

import java.io.File

import dao.MissionDao
import javax.inject.Inject
import org.apache.commons.io.FileUtils
import org.joda.time.DateTime
import models.Tables._
import play.api.mvc.RequestHeader
import tool.Tool

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by yz on 2018/6/13
  */
class MissionExecutor @Inject()(missionId: Int, outDir: File)(implicit request: RequestHeader) {
  var workspaceDir: File = _
  var resultDir: File = _
  var logFile: File = _

  init

  def init = {
    val missionIdDir = new File(outDir, missionId.toString)
    Utils.createDirectoryWhenNoExist(missionIdDir)
    workspaceDir = new File(missionIdDir, "workspace")
    Utils.createDirectoryWhenNoExist(workspaceDir)
    resultDir = new File(missionIdDir, "result")
    Utils.createDirectoryWhenNoExist(resultDir)
    logFile = new File(missionIdDir, "log.txt")
  }


  def exec(shBuffer: ArrayBuffer[String], sf: () => Unit) = {
    Future {
      val execCommand = Utils.callLinuxScript(workspaceDir, shBuffer)
      if (execCommand.isSuccess) {
        sf()
        FileUtils.writeStringToFile(logFile, "Run successed!", "UTF-8")
        "success"
      } else {
        FileUtils.writeStringToFile(logFile, execCommand.getErrStr, "UTF-8")
        "error"
      }
    }

  }

}
