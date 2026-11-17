package pt.rcmartins.loop

import pt.rcmartins.loop.model.ItemType

import java.util.Locale
import scala.util.matching.Regex

/** Preserve the wording while identifying resource references for inline icons. */
object ItemText {
  case class Part(text: String, item: Option[ItemType] = None)

  private lazy val names: Map[String, ItemType] = {
    val canonical = ItemType.all.map(item => item.name.toLowerCase(Locale.ROOT) -> item)
    (canonical ++ Seq(
      "glycerin" -> ItemType.Glycerin,
      "herb soap" -> ItemType.RosemarySoap,
      "herb soaps" -> ItemType.RosemarySoap,
      "rosemary soaps" -> ItemType.RosemarySoap,
      "soap" -> ItemType.RosemarySoap,
      "soaps" -> ItemType.RosemarySoap,
      "lavender" -> ItemType.MagicLavender,
    )).toMap
  }

  private lazy val references: Regex = {
    val terms = names.keys.toSeq.sortBy(name => -name.length).map { name =>
      val suffix =
        if (name == "soap" || name == "soaps") "(?!\\s+(?:mold|shop|business)\\b)" else ""
      Regex.quote(name) + suffix
    }
    ("(?i)(?:\\$\\d+(?:\\.\\d{2})?(?:\\s+coins)?|" +
      terms.mkString("|") + ")").r
  }

  def parts(text: String): Seq[Part] = {
    val result = Vector.newBuilder[Part]
    var offset = 0
    def wordCharacter(c: Char): Boolean = c.isLetterOrDigit || c == '_'
    references
      .findAllMatchIn(text)
      .filter { m =>
        (m.start == 0 || !wordCharacter(text.charAt(m.start - 1))) &&
          (m.end == text.length || !wordCharacter(text.charAt(m.end)))
      }
      .foreach { matched =>
        if (matched.start > offset) result += Part(text.substring(offset, matched.start))
        val item =
          if (matched.matched.startsWith("$")) Some(ItemType.Coins)
          else names.get(matched.matched.toLowerCase(Locale.ROOT))
        result += Part(matched.matched, item)
        offset = matched.end
      }
    if (offset < text.length) result += Part(text.substring(offset))
    result.result()
  }

}
