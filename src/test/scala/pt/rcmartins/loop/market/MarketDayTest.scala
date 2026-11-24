package pt.rcmartins.loop.market

import pt.rcmartins.loop.market.Market._
import zio.Scope
import zio.json._
import zio.test.{Spec, ZIOSpecDefault, assertTrue}

object MarketDayTest extends ZIOSpecDefault {
  private val timber = Offers(0)
  private val oil = Offers(1)
  private val rope = Offers(2)
  private val lantern = Recipes(0)
  private val brace = Recipes(1)

  private def complete(day: MarketDay, job: Job): MarketDay = {
    val started = day.start(job)
    assert(started.jobs.exists(_.id == job.id), s"Could not start ${job.id} at ${day.elapsedMs}: ${day.blocked(job)}")
    started.advance(day.duration(job))
  }

  private def play(day: MarketDay, jobs: Job*): MarketDay = jobs.foldLeft(day)(complete)

  override def spec: Spec[Scope, Any] = suite("The Last Market Day")(
    test("idle time and paused time do not advance the clock") {
      val day = MarketDay()
      val paused = day.start(timber).copy(paused = true)
      assertTrue(day.advance(20000) == day, paused.advance(20000) == paused)
    },
    test("purchases and recipes reserve inputs up front and pay once on completed delivery") {
      val stocked = play(MarketDay(), timber, oil, lantern, lantern)
      val started = stocked.start(Merchant)
      val delivered = started.advance(90000)
      assertTrue(
        stocked.crowns == 5,
        stocked.count(Lantern) == 2,
        started.count(Lantern) == 0,
        started.start(Shelter) == started,
        delivered.crowns == 16,
        delivered.delivered == Set(Merchant.id),
        delivered.start(Merchant) == delivered,
        delivered.elapsedMs < DaySeconds * 1000,
      )
    },
    test("the shelter, crossing, and Highbank form a playable campaign across loops") {
      val shelterDay = play(MarketDay(), timber, oil, lantern, lantern, Shelter)
      val afterShelter = shelterDay.endDay.nextLoop
      val crossingDay = play(afterShelter, timber, rope, brace, brace, Crossing)
      val afterCrossing = crossingDay.endDay.nextLoop
      val highbankDay = play(afterCrossing, timber, timber, oil, rope, lantern, lantern, lantern, brace, brace, Highbank)
      assertTrue(
        !shelterDay.canAutomate(Supply),
        afterShelter.canAutomate(Supply),
        afterShelter.canAutomate(Workshop),
        afterShelter.count(Timber) == 0,
        afterCrossing.canAutomate(Dispatch),
        afterCrossing.crowns == StartingCrowns + 4,
        highbankDay.delivered(Highbank.id),
        highbankDay.elapsedMs <= Highbank.deadline * 1000,
        highbankDay.endDay.nextLoop.milestones(Highbank.id),
      )
    },
    test("the plan can supply, craft, and deliver the final order with parallel jobs") {
      val ready = MarketDay(crowns = 16, milestones = Set(Shelter.id, Crossing.id))
        .setTarget(timber, 2).setTarget(oil, 2).setTarget(rope, 2)
        .setTarget(lantern, 4).setTarget(brace, 2).toggleOrder(Highbank).togglePlan
      val advanced = ready.advance(15000)
      val result = advanced.advance(75000)
      assertTrue(
        advanced.jobs.size >= 2,
        result.delivered(Highbank.id),
        result.stock.values.forall(_ >= 0),
        result.crowns >= 0,
      )
    },
    test("a plan respects the cash reserve and does not spend time blocked") {
      val planned = MarketDay(milestones = Set(Merchant.id)).setReserve(9).setTarget(timber, 2).togglePlan
      val advanced = planned.advance(20000)
      assertTrue(advanced == planned, advanced.waitingFor(Supply).contains("Cash reserved"))
    },
    test("guild support lets a single plan supply the shelter and Highbank in one day") {
      val morning = MarketDay(
        ended = true,
        delivered = Set(Merchant.id),
        milestones = Set(Shelter.id, Crossing.id, Highbank.id),
      ).nextLoop
      val fullDay = morning
        .setTarget(timber, 2)
        .setTarget(oil, 2)
        .setTarget(rope, 2)
        .setTarget(lantern, 4)
        .setTarget(brace, 2)
        .toggleOrder(Shelter)
        .toggleOrder(Highbank)
        .togglePlan
        .advance(90000)
      assertTrue(
        morning.left(timber) == 3,
        morning.crowns == StartingCrowns + GuildAdvance + BridgeAdvance,
        fullDay.bothReliefOrders,
        fullDay.endDay.nextLoop.fullRelief,
      )
    },
    test("an order finishing exactly at its deadline is delivered") {
      val latestStart = (Merchant.deadline - Merchant.seconds) * 1000
      val day = MarketDay(elapsedMs = latestStart, stock = Map(Lantern.id -> 2))
      val onTime = day.start(Merchant).advance(Merchant.seconds * 1000)
      val late = day.copy(elapsedMs = latestStart + 1)
      assertTrue(onTime.delivered(Merchant.id), late.blocked(Merchant).contains("Too late today"))
    },
    test("saves resume paid work and previews use their own storage") {
      val active = MarketDay().start(timber).advance(1000)
      val saved = active.toJson.fromJson[MarketDay]
      assertTrue(
        saved == Right(active),
        saved.exists(_.advance(3000).count(Timber) == 3),
        MarketDay.saveKey("/time-loop/pr-preview/pr-42/") == MarketDay.saveKey("/time-loop/pr-preview/pr-42/index.html"),
        MarketDay.saveKey("/time-loop/pr-preview/pr-42/") != MarketDay.saveKey("/time-loop/"),
        MarketDay.saveKey("/time-loop/") != "save",
      )
    },
  )
}
