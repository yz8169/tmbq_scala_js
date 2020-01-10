package myJs.user

import org.querki.jquery.{$, JQueryAjaxSettings}
import org.querki.jquery.JQuery
import org.scalajs.dom.raw.Element

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import myJs.Utils._
import myJs.myPkg.{Swal, SwalOptions}

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import myJs.utils.Implicits._

/**
  * Created by yz on 2019/4/25
  */
@JSExportTopLevel("ParamAdjust")
object ParamAdjust {

  @JSExport("init")
  def init = {
    bootStrapValidator
    g.$("#form").bootstrapValidator("revalidateField", "missionName")

  }

  @JSExport("compoundChange")
  def compoundChange(element: Element) = {
    val value = $(element).find(">option:selected").`val`()
    val missionId = $("#missionId").find(">option:selected").`val`()
    val url = g.jsRoutes.controllers.MissionController.getArgs().url.toString
    val ajaxSettings = JQueryAjaxSettings.url(s"${url}?missionId=${missionId}&compoundName=${value}").
      `type`("get").contentType("application/json").success { (data, status, e) =>
      val rs = data.asInstanceOf[js.Dictionary[String]]
      $(":input[name='flMin']").`val`(rs("fl").toString)
      $(":input[name='flMax']").`val`(rs("fl").toString)
      $(":input[name='step']").`val`("2")
      ArrayBuffer("snr", "nups", "ndowns", "iteration", "bLine", "rtlw", "rtrw","rt","peakLocation").foreach { x =>
        $(s":input[name='${x}']").`val`(rs(x))
      }
      ArrayBuffer("flMin", "flMax", "step", "snr", "nups", "ndowns", "iteration", "rtlw", "rtrw","rt").foreach { x =>
        g.$("#form").bootstrapValidator("revalidateField", x)
      }
    }
    $.ajax(ajaxSettings)

  }


  @JSExport("bootStrapValidator")
  def bootStrapValidator = {

    def nonEmpty: js.Function = {
      (value: String, validator: js.Any, field: js.Any) => {
        value.nonEmpty
      }

    }

    def oddNumber: js.Function = {
      (value: String, validator: js.Any, field: js.Any) => {
        value.isInt && (value.toInt % 2 == 1)
      }

    }

    def posEvenNumber: js.Function = {
      (value: String, validator: js.Any, field: js.Any) => {
        value.isInt && value.toInt > 0 && (value.toInt % 2 == 0)
      }

    }

    val url = g.jsRoutes.controllers.AdjustMissionController.missionNameCheck().url.toString
    val dict: js.Dictionary[js.Any] = js.Dictionary(
      "framework" -> "bootstrap",
      "icon" -> js.Dictionary(
        "valid" -> "glyphicon glyphicon-ok",
        "invalid" -> "glyphicon glyphicon-remove",
        "validating" -> "glyphicon glyphicon-refresh",
      ),
      "fields" -> js.Dictionary(
        "missionName" -> js.Dictionary(
          "validators" -> js.Dictionary(
            "notEmpty" -> js.Dictionary(
              "message" -> "任务名不能为空！",
            ),
            "remote" -> js.Dictionary(
              "message" -> "任务名已存在!",
              "url" -> url,
              "type" -> "POST",
              "delay" -> 1000,
            )
          )
        ),
        "missionId" -> js.Dictionary(
          "validators" -> js.Dictionary(
            "callback" -> js.Dictionary(
              "message" -> "请选择一个任务！",
              "callback" -> nonEmpty
            )
          )
        ),
        "compoundName" -> js.Dictionary(
          "validators" -> js.Dictionary(
            "callback" -> js.Dictionary(
              "message" -> "请选择一个化合物！",
              "callback" -> nonEmpty
            )
          )
        ),
        "flMin" -> js.Dictionary(
          "validators" -> js.Dictionary(
            "notEmpty" -> js.Dictionary(
              "message" -> "WS4PP最小值不能为空！",
            ),
            "callback" -> js.Dictionary(
              "message" -> "WS4PP最小值只能为奇数！",
              "callback" -> oddNumber,
            )
          )
        ),
        "flMax" -> js.Dictionary(
          "validators" -> js.Dictionary(
            "notEmpty" -> js.Dictionary(
              "message" -> "WS4PP最大值不能为空！",
            ),
            "callback" -> js.Dictionary(
              "message" -> "WS4PP最大值只能为奇数！",
              "callback" -> oddNumber,
            )
          )
        ),
        "step" -> js.Dictionary(
          "validators" -> js.Dictionary(
            "notEmpty" -> js.Dictionary(
              "message" -> "WS4PP步长不能为空！",
            ),
            "callback" -> js.Dictionary(
              "message" -> "WS4PP步长只能为正偶数！",
              "callback" -> posEvenNumber
              ,
            )
          )
        ),
        "nups" -> js.Dictionary(
          "validators" -> js.Dictionary(
            "notEmpty" -> js.Dictionary(
              "message" -> "NUPS4PP不能为空！",
            ),
            "integer" -> js.Dictionary(
              "message" -> "NUPS4PP必须为整数！",
            )
          )
        ),
        "ndowns" -> js.Dictionary(
          "validators" -> js.Dictionary(
            "notEmpty" -> js.Dictionary(
              "message" -> "NDOWNS4PP不能为空！",
            ),
            "integer" -> js.Dictionary(
              "message" -> "NDOWNS4PP必须为整数！",
            )
          )
        ),
        "iteration" -> js.Dictionary(
          "validators" -> js.Dictionary(
            "notEmpty" -> js.Dictionary(
              "message" -> "I4PP不能为空！",
            ),
            "integer" -> js.Dictionary(
              "message" -> "I4PP必须为整数！",
            ),
            "between" -> js.Dictionary(
              "message" -> "I4PP必须大于0！",
              "min" -> 1,
              "max" -> Integer.MAX_VALUE,
            ),
          )
        ),
        "snr" -> js.Dictionary(
          "validators" -> js.Dictionary(
            "notEmpty" -> js.Dictionary(
              "message" -> "SNR4PP不能为空！",
            ),
            "numeric" -> js.Dictionary(
              "message" -> "SNR4PP必须为数字！",
            ),
          )
        ),
        "rtlw" -> js.Dictionary(
          "validators" -> js.Dictionary(
            "notEmpty" -> js.Dictionary(
              "message" -> "RTLW不能为空！",
            ),
            "numeric" -> js.Dictionary(
              "message" -> "RTLW必须为数字！",
            ),
          )
        ),
        "rtrw" -> js.Dictionary(
          "validators" -> js.Dictionary(
            "notEmpty" -> js.Dictionary(
              "message" -> "RTRW不能为空！",
            ),
            "numeric" -> js.Dictionary(
              "message" -> "RTRW必须为数字！",
            ),
          )
        ),
        "rt" -> js.Dictionary(
          "validators" -> js.Dictionary(
            "notEmpty" -> js.Dictionary(
              "message" -> "RT不能为空！",
            ),
            "numeric" -> js.Dictionary(
              "message" -> "RT必须为数字！",
            ),
          )
        ),
      )
    )
    g.$(s"#form").bootstrapValidator(dict)

  }


}
