package pt.rcmartins.loop.market

object Market {
  val DaySeconds = 90
  val StartingCrowns = 12
  val GuildAdvance = 6
  val BridgeAdvance = 4

  final case class Good(id: String, name: String, icon: String)
  final case class Stack(good: Good, quantity: Int)

  val Timber = Good("timber", "Timber", "🪵")
  val Oil = Good("oil", "Lamp oil", "🫙")
  val Lantern = Good("lantern", "Lanterns", "🏮")
  val Rope = Good("rope", "Rope", "🧵")
  val Brace = Good("brace", "Beam braces", "🪚")
  val Goods = Vector(Timber, Oil, Lantern, Rope, Brace)

  sealed abstract class Lane(val id: String)
  case object Supply extends Lane("supply")
  case object Workshop extends Lane("workshop")
  case object Dispatch extends Lane("dispatch")
  val Lanes = Vector(Supply, Workshop, Dispatch)

  sealed trait Job {
    def id: String
    def title: String
    def lane: Lane
    def seconds: Int
    def input: Vector[Stack]
    def output: Vector[Stack]
    def unlock: Option[String]
  }

  final case class Offer(
      id: String,
      title: String,
      description: String,
      output: Vector[Stack],
      price: Int,
      seconds: Int,
      batches: Int,
      unlock: Option[String] = None,
      extraBatchAfter: Option[String] = None
  ) extends Job {
    val lane: Lane = Supply
    val input = Vector.empty[Stack]
  }

  final case class Recipe(
      id: String,
      title: String,
      input: Vector[Stack],
      output: Vector[Stack],
      seconds: Int,
      unlock: Option[String] = None
  ) extends Job {
    val lane: Lane = Workshop
  }

  final case class Order(
      id: String,
      title: String,
      buyer: String,
      input: Vector[Stack],
      reward: Int,
      seconds: Int,
      deadline: Int,
      milestone: String,
      unlock: Option[String] = None
  ) extends Job {
    val lane: Lane = Dispatch
    val output = Vector.empty[Stack]
  }

  val Merchant = Order(
    "merchant",
    "Light the market stalls",
    "The market guild",
    Vector(Stack(Lantern, 2)),
    11,
    7,
    55,
    s"Next loop: +$GuildAdvance starting crowns and one extra timber batch."
  )

  val Shelter = Order(
    "shelter",
    "Light the hill shelter",
    "Mara at the shelter",
    Vector(Stack(Lantern, 2)),
    5,
    7,
    75,
    "Next loop: unlock crafting automation and bridge orders."
  )

  val Crossing = Order(
    "crossing",
    "Brace the low bridge",
    "The bridge crew",
    Vector(Stack(Brace, 2)),
    8,
    8,
    72,
    "Next loop: unlock automatic delivery and Highbank orders.",
    Some(Shelter.id)
  )

  val Highbank = Order(
    "highbank",
    "Supply the evacuation",
    "Highbank village",
    Vector(Stack(Lantern, 3), Stack(Brace, 2)),
    15,
    10,
    90,
    "Completes the Highbank evacuation.",
    Some(Crossing.id)
  )

  val Offers = Vector(
    Offer(
      "timber-cart",
      "Timber",
      "Sawmill",
      Vector(Stack(Timber, 3)),
      4,
      4,
      2,
      extraBatchAfter = Some(Merchant.id)
    ),
    Offer("oil-crate", "Lamp oil", "Chandler", Vector(Stack(Oil, 3)), 3, 4, 2),
    Offer(
      "rope-bundle",
      "Rope",
      "Bridge crew",
      Vector(Stack(Rope, 2)),
      4,
      5,
      2,
      Some(Shelter.id)
    ),
  )

  val Recipes = Vector(
    Recipe(
      "make-lantern",
      "Lantern",
      Vector(Stack(Timber, 1), Stack(Oil, 1)),
      Vector(Stack(Lantern, 1)),
      6
    ),
    Recipe(
      "make-brace",
      "Beam brace",
      Vector(Stack(Timber, 1), Stack(Rope, 1)),
      Vector(Stack(Brace, 1)),
      7,
      Some(Shelter.id)
    ),
  )

  val Orders = Vector(Merchant, Shelter, Crossing, Highbank)
  val Jobs: Map[String, Job] = (Offers ++ Recipes ++ Orders).map(job => job.id -> job).toMap
}
