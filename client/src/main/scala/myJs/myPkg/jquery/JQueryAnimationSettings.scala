package myJs.myPkg.jquery

/**
 * Created by Administrator on 2019/12/17
 */

import myJs.myPkg.jsext._
import org.scalajs.dom
import org.scalajs.dom.Element

import scala.scalajs.js
import scala.scalajs.js.|

@js.native
trait JQueryAnimationSettings extends js.Object {
  // TODO: fill these in if anybody actually cares...
}

object JQueryAnimationSettings extends JQueryAnimationSettingsBuilder(noOpts)
class JQueryAnimationSettingsBuilder(val dict:OptMap) extends JSOptionBuilder[JQueryAnimationSettings, JQueryAnimationSettingsBuilder](new JQueryAnimationSettingsBuilder(_)) {
  def duration(v:Number | String) = jsOpt("duration", v)
  def easing(v:String) = jsOpt("easing", v)
  def queue(v:Boolean | String) = jsOpt("queue", v)
  def specialEasing(v:js.Object) = jsOpt("specialEasing", v)
  // TODO: This one's problematic. The second param of the callback is a "Tween", but I can't find the
  // documentation on what the heck that is.
  def step(v:js.ThisFunction2[Element, Number, js.Any, js.Any]) = jsOpt("step", v)
  def progress(v:js.ThisFunction3[Element, JQueryPromise, Number, Number, Any]) = jsOpt("progress", v)
  def complete(v:js.ThisFunction0[Element, Any]) = jsOpt("complete", v)
  def start(v:js.ThisFunction1[Element, JQueryPromise, Any]) = jsOpt("start", v)
  def done(v:js.ThisFunction2[Element, JQueryPromise, Boolean, Any]) = jsOpt("done", v)
  def fail(v:js.ThisFunction2[Element, JQueryPromise, Boolean, Any]) = jsOpt("fail", v)
  def always(v:js.ThisFunction2[Element, JQueryPromise, Boolean, Any]) = jsOpt("always", v)
}