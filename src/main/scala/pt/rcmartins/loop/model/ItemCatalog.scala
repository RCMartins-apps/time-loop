package pt.rcmartins.loop.model

import pt.rcmartins.loop.Constants

/** Edit presentation here; numeric item IDs remain stable for existing saves. */
object ItemCatalog {
  case class Presentation(
      name: String,
      iconPath: String,
      iconColor: String,
      aliases: Seq[String] = Seq.empty
  )

  private val items: Map[Int, Presentation] = Map(
    1 -> Presentation("Coins", Constants.Icons.TwoCoins, "text-yellow-300", Seq()),
    2 -> Presentation("Rosemary", Constants.Icons.Olive, "text-green-500", Seq()),
    3 -> Presentation(
      "Glycerin Based Soap",
      Constants.Icons.StoneBlock,
      "text-stone-50",
      Seq("glycerin")
    ),
    4 -> Presentation("Melted Glycerin", Constants.Icons.BubblingBowl, "text-neutral-300", Seq()),
    5 -> Presentation(
      "Rosemary Soap",
      Constants.Icons.Soap,
      "text-green-300",
      Seq("herb soap", "herb soaps", "rosemary soaps", "soap", "soaps")
    ),
    6 -> Presentation("Magic Lavender", Constants.Icons.Olive, "text-purple-300", Seq("lavender")),
    14 -> Presentation("Magic Lavender Soap", Constants.Icons.Soap, "text-purple-300", Seq()),
    7 -> Presentation("Frozen Momo", Constants.Icons.DumplingBao, "text-cyan-500", Seq()),
    8 -> Presentation("Rice", Constants.Icons.BowlOfRice, "text-stone-50", Seq()),
    9 -> Presentation("Cherries", Constants.Icons.Cherry, "text-red-600", Seq()),
    10 -> Presentation("Momo", Constants.Icons.DumplingBao, "text-yellow-100", Seq()),
    11 -> Presentation("Noodles", Constants.Icons.Noodles, "text-yellow-400", Seq()),
  )

  def apply(id: Int): Presentation = items(id)
}
