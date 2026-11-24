package pt.rcmartins.loop

import com.raquo.laminar.api.L._
import org.scalajs.dom
import pt.rcmartins.loop.market.{MarketDay, MarketView}
import zio.json._

import scala.scalajs.js.timers.setInterval
import scala.util.Try

object Main {
  def main(args: Array[String]): Unit = {
    val key = MarketDay.saveKey(dom.window.location.pathname)
    val stored = Try(Option(dom.window.localStorage.getItem(key)))
    val loaded = stored.toOption.flatten.flatMap(_.fromJson[MarketDay].toOption)
    val state = Var(loaded.getOrElse(MarketDay()))
    val saveWarning = Var(stored.isFailure)
    var saved = loaded

    def save(): Unit = {
      val current = state.now()
      if (!saved.contains(current)) {
        val result = Try(dom.window.localStorage.setItem(key, current.toJson))
        saveWarning.set(result.isFailure)
        if (result.isSuccess) saved = Some(current)
      }
    }

    var last = dom.window.performance.now()
    def updateClock(): Unit = {
      val now = dom.window.performance.now()
      val elapsed = (now - last).toInt
      last += elapsed
      if (!dom.document.hidden) {
        val current = state.now()
        val updated = current.advance(elapsed)
        if (updated != current) state.set(updated)
      }
    }

    dom.document.addEventListener(
      "visibilitychange",
      (_: dom.Event) => {
        last = dom.window.performance.now()
        if (dom.document.hidden) save()
      }
    )
    dom.window.addEventListener("pagehide", (_: dom.Event) => save())
    setInterval(100)(updateClock())
    setInterval(1000)(save())

    render(dom.document.getElementById("main-div"), new MarketView(state, saveWarning.signal).view)
  }

}
