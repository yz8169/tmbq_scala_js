package myJs

import com.karasiq.bootstrap.Bootstrap.default._
import org.querki.jquery._
import org.singlespaced.d3js.Ops._
import org.singlespaced.d3js.d3

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

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
