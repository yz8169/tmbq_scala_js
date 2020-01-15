package controllers

import java.io.File

import javax.inject.Inject
import org.apache.commons.io.FileUtils
import org.apache.commons.lang3.StringUtils
import play.api.mvc.{AbstractController, ControllerComponents}
import tool.Tool
import utils.Utils

import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Failure

/**
  * Created by yz on 2018/11/14
  */
class TestController @Inject()(cc: ControllerComponents, tool: Tool) extends AbstractController(cc) {

  def test = Action { implicit request =>
    val parent=tool.createTempDirectory("tmpDir")
    val regressFile=new File(parent,"regress.txt")
    val regressExcelFile=new File(parent,"regress.xlsx")
    val colorFile=new File(parent,"color.txt")
    tool.dye(regressFile,colorFile,regressExcelFile)
    Ok("success! ")
  }

}
