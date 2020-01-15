package utils

import java.io.File

import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.collection.parallel.{ForkJoinTaskSupport, ParSeq}

/**
  * Created by yz on 2019/4/25
  */
object Implicits {

  implicit class MyFile(file: File) {

    def toUnixPath = {
      val path = file.getAbsolutePath
      path.toUnixPath
    }

  }

  implicit class MyString(v: String) {

    def toUnixPath = {
      v.replace("\\", "/").replaceAll("D:", "/mnt/d").
        replaceAll("E:", "/mnt/e").replaceAll("C:", "/mnt/c")

    }

    def startWithsIgnoreCase(prefix: String) = {
      v.toLowerCase.startsWith(prefix.toLowerCase)

    }

    def toFile(file: File, encoding: String = "UTF-8", append: Boolean = false) = {
      FileUtils.writeStringToFile(file, v, encoding, append)

    }

  }


}
