package utils

import java.io.File

import javax.inject.Inject
import org.apache.commons.io.FileUtils
import org.joda.time.DateTime
import play.api.mvc.RequestHeader
import utils.Pojo.CommandData

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ParSeq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import utils.Implicits._

import scala.reflect.ClassTag

/**
  * Created by yz on 2018/6/13
  */
class CommandExecutor(logFile: File) {

  init

  var isSuccess: Boolean = true

  def init={
    "Run successed!".toFile(logFile)
  }

  def exec(commandDatas: ParSeq[CommandData]):CommandExecutor = {
    if (isSuccess) {
      val b = commandDatas.forall { commandData =>
        val execCommand = Utils.callLinuxScript(commandData.workspace, commandData.command)
        printLog(execCommand)
        execCommand.isSuccess
      }
      if (!b) isSuccess = false
    }
    this

  }

  def exec(f: () => ParSeq[CommandData]):CommandExecutor = {
    if (isSuccess) {
      val commandDatas = f()
      exec(commandDatas)
    }
    this

  }

  def exec(commandData: CommandData):CommandExecutor = {
    if (isSuccess) {
      val execCommand = Utils.callLinuxScript(commandData.workspace, commandData.command)
      printLog(execCommand)
      val b = execCommand.isSuccess
      if (!b) isSuccess = false
    }
    this

  }

  def exec[X: ClassTag](f: () => CommandData):CommandExecutor = {
    if (isSuccess) {
      val commandData = f()
      exec(commandData)
    }
    this

  }

  def printLog(execCommand: ExecCommand) = {
    if (!execCommand.isSuccess) {
      execCommand.getErrStr.toFile(logFile)
    }

  }


}
