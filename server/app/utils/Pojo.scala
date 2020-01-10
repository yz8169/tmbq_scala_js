package utils

import java.io.File

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Administrator on 2019/7/1
  */

object Pojo {

  case class CommandData(workspace: File, command: ArrayBuffer[String])

  case class IndexData(index: String, compoundName: String)



}



