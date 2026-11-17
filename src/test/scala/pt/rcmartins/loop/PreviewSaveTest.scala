package pt.rcmartins.loop

import zio.Scope
import zio.test.{assertTrue, Spec, ZIOSpecDefault}

object PreviewSaveTest extends ZIOSpecDefault {
  override def spec: Spec[Scope, Any] = suite("Preview saves")(
    test("production keeps existing saves") {
      assertTrue(SaveLoadImpl.saveKey("/time-loop/") == "save", SaveLoadImpl.saveKey("/") == "save")
    },
    test("previews cannot overwrite production or another PR's save") {
      val first = SaveLoadImpl.saveKey("/time-loop/pr-preview/pr-9/")
      val second = SaveLoadImpl.saveKey("/time-loop/pr-preview/pr-10/")
      assertTrue(first != "save", second != "save", first != second)
    },
  )
}
