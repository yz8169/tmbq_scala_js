package controllers

import java.io.File

import javax.inject.Inject
import org.apache.commons.lang3.StringUtils
import org.zeroturnaround.zip.ZipUtil
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents}
import tool.Tool
import utils.Utils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ArrayBuffer

/**
  * Created by yz on 2018/10/22
  */
class ToolController @Inject()(cc: ControllerComponents, tool: Tool, formTool: FormTool) extends AbstractController(cc) {

  def unitConversionBefore = Action { implicit request =>
    Ok(views.html.user.unitConversion())
  }

  def unitConversion = Action(parse.multipartFormData) { implicit request =>
    val data = formTool.unitConversionForm.bindFromRequest().get
    val tmpDir = tool.createTempDirectory("tmpDir")
    val file = new File(tmpDir, "data.xlsx")
    val tmpFile = request.body.file("file").get
    tmpFile.ref.moveTo(file, replace = true)
    val unit = data.unit
    val toUnit = data.toUnit
    val standardUnit = tool.standardUnit
    val noMwUnits = ArrayBuffer("nM", "uM", "mM", "nmol/g", "nmol/mg", "pmol/mg", "umol/mg", "umol/g")
    val mwUnits = ArrayBuffer("g/mL", "mg/mL", "mg/g", "ug/mL", "ug/g", "ug/uL", "ug/L", "ng/g", "ppm", "ppb", "ng/mL", "ppt")
    val noMwMap = tool.getNoMwMap
    val lines = Utils.xlsx2Lines(file).map(_.split("\t"))

    val newLines = ArrayBuffer(lines.head)
    newLines += lines(1)
    var message = ""
    try {
      newLines ++= lines.drop(2).zipWithIndex.map { case (columns, i) =>
        columns.take(data.fromC - 1) ++ columns.drop(data.fromC - 1).zipWithIndex.map { case (value, tmpj) =>
          val j = tmpj + data.fromC - 1
          val unitArray = ArrayBuffer(unit, toUnit)
          val mw = if (unitArray.intersect(noMwUnits).size == 2 || unitArray.intersect(mwUnits).size == 2) {
            1
          } else {
            val value = lines(1)(j)
            if (!Utils.isDouble(value)) {
              message = s"文件第2行第${j + 1}列(${value})不为数字"
            }
            value.toDouble
          }
          val mwMap = tool.getmwMap(mw)
          if (StringUtils.isBlank(value)) {
            value
          } else {
            if (!Utils.isDouble(value)) {
              message = s"文件第${i + 3}行第${j + 1}列(${value})不为数字"
            }
            var finalValue = value.toDouble
            if (noMwUnits.contains(unit)) {
              finalValue *= noMwMap((unit, standardUnit))
            } else {
              finalValue *= mwMap((unit, standardUnit))
            }
            if (noMwUnits.contains(toUnit)) {
              finalValue *= noMwMap((standardUnit, toUnit))
            } else {
              finalValue *= mwMap((standardUnit, toUnit))
            }
            finalValue.toString
          }


        }
      }
      val resultFile = new File(tmpDir, "result.xlsx")
      Utils.lines2Xlsx(newLines.map(_.mkString("\t")), resultFile)
      val base64 = Utils.getBase64Str(resultFile)
      tool.deleteDirectory(tmpDir)
      Ok(Json.obj("valid" -> true, "data" -> base64)).as("application/json")
    } catch {
      case x: Exception =>
        Ok(Json.obj("valid" -> false, "message" -> message)).as("application/json")

    }

  }

  def downloadExampleData = Action {
    implicit request =>
      val data = formTool.fileNameForm.bindFromRequest().get
      val exampleDir = Utils.exampleFile
      val resultFile = new File(exampleDir, data.fileName)
      Ok.sendFile(resultFile).withHeaders(
        CACHE_CONTROL -> "max-age=3600",
        CONTENT_DISPOSITION -> s"attachment; filename=${
          resultFile.getName
        }",
        CONTENT_TYPE -> "application/x-download"
      )
  }

}
