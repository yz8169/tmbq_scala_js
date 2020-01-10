package myJs


import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import myJs.myPkg.jquery._

/**
  * Created by yz on 2019/4/25
  */
@JSExportTopLevel("App")
object App {

  @JSExport("init")
  def init={
    val shareTitle="TMBQ"
    val beforeTitle=$("#shareTitle").text()
    $("#shareTitle").text(s"${beforeTitle}-${shareTitle}")

  }

}
