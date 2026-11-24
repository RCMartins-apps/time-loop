package pt.rcmartins.loop.market

import pt.rcmartins.loop.market.Market._
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

final case class RunningJob(id: String, endMs: Int, automatic: Boolean, durationMs: Int)
object RunningJob {
  implicit val decoder: JsonDecoder[RunningJob] = DeriveJsonDecoder.gen[RunningJob]
  implicit val encoder: JsonEncoder[RunningJob] = DeriveJsonEncoder.gen[RunningJob]
}

final case class DayLog(atMs: Int, text: String)
object DayLog {
  implicit val decoder: JsonDecoder[DayLog] = DeriveJsonDecoder.gen[DayLog]
  implicit val encoder: JsonEncoder[DayLog] = DeriveJsonEncoder.gen[DayLog]
}

final case class MarketDay(
    loop: Int = 1,
    elapsedMs: Int = 0,
    crowns: Int = StartingCrowns,
    stock: Map[String, Int] = Map.empty,
    purchases: Map[String, Int] = Map.empty,
    delivered: Set[String] = Set.empty,
    milestones: Set[String] = Set.empty,
    todayWork: Map[String, Int] = Map.empty,
    allWork: Map[String, Int] = Map.empty,
    jobs: Vector[RunningJob] = Vector.empty,
    targets: Map[String, Int] = Map.empty,
    autoOrders: Set[String] = Set.empty,
    reserve: Int = 0,
    planRunning: Boolean = false,
    paused: Boolean = false,
    ended: Boolean = false,
    ledger: Vector[DayLog] = Vector.empty,
    fullRelief: Boolean = false,
) {
  def count(good: Good): Int = stock.getOrElse(good.id, 0)
  def has(job: Job): Boolean = job.unlock.forall(milestones.contains)
  def canAutomate(lane: Lane): Boolean = lane match {
    case Supply   => milestones.nonEmpty
    case Workshop => milestones.contains(Shelter.id)
    case Dispatch => milestones.contains(Crossing.id)
  }

  def todayLevel(lane: Lane): Int = todayWork.getOrElse(lane.id, 0) / 3
  def permanentLevel(lane: Lane): Int = allWork.getOrElse(lane.id, 0) / 6
  def speed(lane: Lane): Double = 1.0 + todayLevel(lane) * 0.10 + permanentLevel(lane) * 0.01
  def duration(job: Job): Int = math.ceil(job.seconds * 1000 / speed(job.lane)).toInt
  def left(offer: Offer): Int = {
    val extra = if (offer.extraBatchAfter.exists(milestones.contains)) 1 else 0
    offer.batches + extra - purchases.getOrElse(offer.id, 0)
  }

  def bothReliefOrders: Boolean = delivered(Shelter.id) && delivered(Highbank.id)
  def timeLeftMs: Int = DaySeconds * 1000 - elapsedMs
  def planConfigured: Boolean = targets.values.exists(_ > 0) || autoOrders.nonEmpty
  def workIn(lane: Lane): Option[RunningJob] = jobs.find(job => Jobs(job.id).lane == lane)

  def blocked(job: Job, automatic: Boolean = false): Option[String] = {
    if (ended) return Some("The day has ended")
    if (!has(job) || (automatic && !canAutomate(job.lane))) return Some("Locked")
    val deadline = job match {
      case order: Order => order.deadline * 1000
      case _            => DaySeconds * 1000
    }
    job match {
      case offer: Offer if left(offer) <= 0    => return Some("Sold out today")
      case order: Order if delivered(order.id) => return Some("Delivered")
      case _                                   =>
    }
    if (workIn(job.lane).nonEmpty) return Some("Station busy")
    if (elapsedMs + duration(job) > deadline) return Some("Too late today")
    job match {
      case offer: Offer if crowns - (if (automatic) reserve else 0) < offer.price =>
        return Some(
          if (automatic && crowns >= offer.price) "Cash reserved"
          else s"Need ${offer.price - crowns} more crowns"
        )
      case _ =>
    }
    job.input
      .find(stack => count(stack.good) < stack.quantity)
      .map { stack =>
        s"Need ${stack.quantity - count(stack.good)} more ${stack.good.name.toLowerCase}"
      }
      .orElse(
        Option
          .when(!automatic && jobs.exists(job => !job.automatic))("Finish your current job first")
      )
  }

  def start(job: Job): MarketDay =
    if (paused || blocked(job).nonEmpty) this else begin(job, automatic = false)

  private def changeStock(items: Vector[Stack], direction: Int): Map[String, Int] =
    items.foldLeft(stock) { case (inventory, Stack(good, quantity)) =>
      inventory.updated(good.id, inventory.getOrElse(good.id, 0) + quantity * direction)
    }

  private def begin(job: Job, automatic: Boolean): MarketDay = {
    val cost = job match { case offer: Offer => offer.price; case _ => 0 }
    val bought = job match {
      case offer: Offer => purchases.updated(offer.id, purchases.getOrElse(offer.id, 0) + 1)
      case _            => purchases
    }
    val workMs = duration(job)
    copy(
      crowns = crowns - cost,
      stock = changeStock(job.input, -1),
      purchases = bought,
      jobs = jobs :+ RunningJob(job.id, elapsedMs + workMs, automatic, workMs)
    )
  }

  def setTarget(job: Job, quantity: Int): MarketDay =
    if (canAutomate(job.lane) && has(job) && job.output.nonEmpty && Set(0, 2, 4, 6)(quantity))
      copy(targets = if (quantity == 0) targets - job.id else targets.updated(job.id, quantity))
    else this

  def toggleOrder(order: Order): MarketDay =
    if (has(order) && canAutomate(Dispatch))
      copy(autoOrders = if (autoOrders(order.id)) autoOrders - order.id else autoOrders + order.id)
    else this

  def setReserve(value: Int): MarketDay =
    if (canAutomate(Supply) && Set(0, 3, 6, 9)(value)) copy(reserve = value) else this

  def togglePlan: MarketDay =
    if (ended || (!planRunning && !planConfigured)) this else copy(planRunning = !planRunning)

  private def eligibleForPlan(job: Job): Boolean = job match {
    case order: Order => autoOrders(order.id)
    case _            => job.output.exists(s => count(s.good) < targets.getOrElse(job.id, 0))
  }

  def waitingFor(lane: Lane): Option[String] = {
    val candidates: Vector[Job] = lane match {
      case Dispatch => Orders.filter(order => !delivered(order.id)).sortBy(_.deadline)
      case Workshop => Recipes
      case Supply   => Offers
    }
    candidates
      .find(job => has(job) && eligibleForPlan(job))
      .flatMap(job => blocked(job, automatic = true))
  }

  private def schedule: MarketDay = {
    if (!planRunning) return this
    val lanes = Vector[Vector[Job]](Orders.sortBy(_.deadline), Recipes, Offers)
    lanes.foldLeft(this) { (state, candidates) =>
      candidates
        .find(job => state.eligibleForPlan(job) && state.blocked(job, automatic = true).isEmpty)
        .fold(state)(state.begin(_, automatic = true))
    }
  }

  private def finish(work: RunningJob): MarketDay = {
    val job = Jobs(work.id)
    val lane = job.lane.id
    val result = copy(
      stock = changeStock(job.output, 1),
      jobs = jobs.filterNot(_ == work),
      todayWork = todayWork.updated(lane, todayWork.getOrElse(lane, 0) + 1),
      allWork = allWork.updated(lane, allWork.getOrElse(lane, 0) + 1)
    )
    job match {
      case order: Order =>
        val first = !milestones(order.id)
        val note = if (first) s" ${order.milestone}" else ""
        result
          .copy(crowns = crowns + order.reward, delivered = delivered + order.id)
          .record(s"${order.buyer} paid ${order.reward} crowns.$note")
      case _ =>
        result.record(
          job.output
            .map(stack => s"${stack.quantity} ${stack.good.name.toLowerCase}")
            .mkString(", ") + " added to stock."
        )
    }
  }

  private def record(text: String): MarketDay =
    copy(ledger = (ledger :+ DayLog(elapsedMs, text)).takeRight(8))

  /** Resolve completion events in order so a slow frame cannot skip deliveries or deadlines. */
  def advance(deltaMs: Int): MarketDay = {
    @annotation.tailrec
    def run(state: MarketDay, remaining: Int): MarketDay = {
      val scheduled = state.schedule
      if (scheduled.jobs.isEmpty || remaining <= 0) scheduled
      else {
        val until = scheduled.jobs.map(_.endMs).min
        val step = math.min(remaining, until - scheduled.elapsedMs)
        val tick = scheduled.copy(elapsedMs = scheduled.elapsedMs + step)
        val finished =
          tick.jobs.filter(_.endMs <= tick.elapsedMs).foldLeft(tick)((day, job) => day.finish(job))
        if (finished.timeLeftMs == 0) finished.endDay
        else if (step == remaining) finished.schedule
        else run(finished, remaining - step)
      }
    }
    if (paused || ended || deltaMs <= 0) this else run(this, deltaMs)
  }

  def endDay: MarketDay =
    if (ended || (elapsedMs == 0 && jobs.isEmpty)) this
    else copy(jobs = Vector.empty, ended = true, planRunning = false, paused = false)

  def nextLoop: MarketDay =
    if (!ended) this
    else {
      val remembered = milestones ++ delivered
      val guildAdvance = if (remembered(Merchant.id)) GuildAdvance else 0
      val reliefAdvance = if (remembered(Crossing.id)) BridgeAdvance else 0
      MarketDay(
        loop = loop + 1,
        crowns = StartingCrowns + guildAdvance + reliefAdvance,
        milestones = remembered,
        allWork = allWork,
        targets = targets,
        autoOrders = autoOrders,
        reserve = reserve,
        fullRelief = fullRelief || bothReliefOrders,
      )
    }

}

object MarketDay {
  implicit val decoder: JsonDecoder[MarketDay] = DeriveJsonDecoder.gen[MarketDay]
  implicit val encoder: JsonEncoder[MarketDay] = DeriveJsonEncoder.gen[MarketDay]

  def saveKey(pathname: String): String = {
    val root = pathname.stripSuffix("/index.html").stripSuffix("/")
    if (root.contains("/pr-preview/pr-")) s"time-loop:last-market-day:$root"
    else "time-loop:last-market-day"
  }

}
