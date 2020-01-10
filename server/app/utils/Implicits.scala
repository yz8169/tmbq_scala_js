package utils

import java.io.File

import org.apache.commons.io.FileUtils

import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.collection.parallel.{ForkJoinTaskSupport, ParSeq}
import scala.collection.parallel.mutable.ParArray
import scala.concurrent.forkjoin.ForkJoinPool

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

  implicit class MyLines(lines: mutable.Buffer[String]) {

    def toLowerCase = {
      lines.map(_.toLowerCase)
    }

    def headers = lines.head.split("\t")

    def lineMap = {
      lines.drop(1).map { line =>
        val columns = line.split("\t")
        headers.map(_.toLowerCase).zip(columns).toMap
      }

    }

    def toFile(file: File, append: Boolean = false, encoding: String = "UTF-8") = {
      FileUtils.writeLines(file, encoding, lines.asJava, append)

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

  implicit class MyParArray[T](pars: ParSeq[T]) {

    def threadNum(t: Int) = {
      pars.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(t))
      pars

    }

  }


}
