package myJs.utils

import java.io.File

/**
  * Created by Administrator on 2019/7/2
  */
object Implicits {

  implicit class MyString(v: String) {

    def isInt: Boolean = {
      try {
        val double = v.toDouble
        double == double.toInt
      } catch {
        case _: Exception =>
          false
      }
    }

  }

}
