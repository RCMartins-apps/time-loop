package pt.rcmartins.loop

import pt.rcmartins.loop.model.ItemType
import zio.Scope
import zio.test.{assertTrue, Spec, ZIOSpecDefault}

object ItemTextTest extends ZIOSpecDefault {
  override def spec: Spec[Scope, Any] = suite("Item descriptions")(
    test("recipe ingredients preserve wording and use the correct icons") {
      val text = "Cost: 1 Melted Glycerin and 1 Rosemary."
      val parts = ItemText.parts(text)
      assertTrue(parts.map(_.text).mkString == text,
        parts.flatMap(_.item) == Seq(ItemType.MeltedGlycerin, ItemType.Rosemary))
    },
    test("long names match before the shorter names they contain") {
      val items = ItemText.parts("Frozen Momo, Momo, Magic Lavender Soap, Rosemary Soap").flatMap(_.item)
      assertTrue(items == Seq(ItemType.FrozenMomo, ItemType.Momo, ItemType.MagicLavenderSoap, ItemType.RosemarySoap))
    },
    test("currency, trade aliases, and lowercase resource names are recognized") {
      val items = ItemText.parts("Trade 5 Herb Soaps for 1 glycerin; Cost: $1.00 Coins").flatMap(_.item)
      assertTrue(items == Seq(ItemType.RosemarySoap, ItemType.Glycerin, ItemType.Coins))
    },
    test("ordinary words and locations are not treated as inventory items") {
      val parts = ItemText.parts("Price: visit the Soap Shop, find the Soap Mold, and start a soap business.")
      assertTrue(parts.flatMap(_.item).isEmpty)
    },
    test("every resource has an icon") {
      assertTrue(ItemType.all.nonEmpty, ItemType.all.forall(_.iconPath.nonEmpty))
    },
  )
}
