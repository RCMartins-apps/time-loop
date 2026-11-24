package pt.rcmartins.loop.market

import com.raquo.laminar.api.L._
import pt.rcmartins.loop.market.Market._

final class MarketView(day: Var[MarketDay], saveWarning: Signal[Boolean]) {
  private def watch[A](value: MarketDay => A): Signal[A] = day.signal.map(value).distinct

  private def seconds(ms: Int): Int = (ms.max(0) + 999) / 1000

  private def clock(ms: Int): String = {
    val total = seconds(ms)
    f"${total / 60}%d:${total % 60}%02d"
  }

  private def workbenchVisible(state: MarketDay): Boolean =
    state.milestones.nonEmpty || state.allWork.getOrElse(Workshop.id, 0) > 0 ||
      state.workIn(Workshop).nonEmpty || Recipes.exists(recipe =>
        state.has(recipe) &&
          recipe.input.forall(input => state.count(input.good) >= input.quantity)
      )

  private def ordersVisible(state: MarketDay): Boolean =
    state.milestones.nonEmpty || state.count(Lantern) >= Merchant.input.head.quantity ||
      state.workIn(Dispatch).nonEmpty || state.delivered.nonEmpty

  private def nextStep(state: MarketDay): String = {
    if (state.paused) "Resume to keep working."
    else if (state.fullRelief || state.bothReliefOrders)
      "Both relief orders delivered. The town is ready for the flood."
    else if (state.delivered(Highbank.id))
      "Highbank is supplied. Deliver to the hill shelter this loop to finish the relief effort."
    else if ((state.delivered -- state.milestones).nonEmpty)
      "Order delivered. End the day when you're ready to use the new unlocks."
    else if (state.jobs.nonEmpty) "Work in progress."
    else if (state.milestones(Highbank.id)) {
      if (state.milestones(Merchant.id))
        "Deliver to the hill shelter and Highbank in the same loop."
      else "Light the market stalls to get extra cash and timber next loop."
    } else if (state.milestones(Crossing.id)) "Deliver 3 lanterns and 2 beam braces to Highbank."
    else if (state.milestones(Shelter.id))
      "Make 2 beam braces for the bridge crew. Buy rope to get started."
    else if (ordersVisible(state) && state.count(Lantern) >= 2) "Pick an order for your 2 lanterns."
    else if (state.count(Timber) == 0) "Buy timber to start making lanterns."
    else if (state.count(Oil) == 0) "Buy lamp oil for the lanterns."
    else "Make 2 lanterns to open your first orders."
  }

  private def icon(good: Good): HtmlElement =
    span(cls := "resource-icon", aria.hidden := true, good.icon)

  private def amounts(items: Vector[Stack]): Seq[HtmlElement] = items.map { stack =>
    span(cls := "amount", icon(stack.good), s"${stack.good.name} ×${stack.quantity}")
  }

  private def stock: HtmlElement =
    ul(
      cls := "stock",
      aria.label := "Stockroom",
      Goods.map { good =>
        val visible = watch { state =>
          if (good == Lantern) workbenchVisible(state)
          else if (good == Rope || good == Brace) state.milestones(Shelter.id)
          else true
        }
        child.maybe <-- visible.map(show =>
          Option.when(show)(
            li(
              icon(good),
              span(good.name),
              strong(child.text <-- watch(state => state.count(good).toString))
            )
          )
        )
      }
    )

  private def header: HtmlElement = headerTag(
    cls := "topbar",
    div(
      cls := "topbar__main",
      h1("The Last Market Day"),
      detailsTag(
        cls := "settings",
        summaryTag("Settings"),
        div(
          cls := "settings-menu",
          button(
            tpe := "button",
            cls := "button",
            dataAttr("install-action") := "install",
            "Install game"
          ),
          detailsTag(
            summaryTag("About the loop"),
            p(
              "The river floods at the end of the day. Each new loop starts with fresh stock and cash. Trade unlocks, skill levels, and automation rules carry over."
            ),
            p("Progress saves on this device. The clock pauses when you switch away."),
          ),
        ),
      ),
    ),
    div(
      cls := "status",
      strong(child.text <-- watch(state => s"${state.crowns} crowns")),
      span(child.text <-- watch(state => s"Loop ${state.loop}")),
      span(child.text <-- watch(state => s"${clock(state.timeLeftMs)} left")),
    ),
    stock,
  )

  private def dayControls: HtmlElement = div(
    cls := "day-controls",
    child.maybe <-- watch(state => state.jobs.nonEmpty || state.paused).map(show =>
      Option.when(show)(
        button(
          tpe := "button",
          cls := "button button--quiet",
          child.text <-- watch(state => if (state.paused) "Resume" else "Pause"),
          onClick --> (_ => day.update(state => state.copy(paused = !state.paused))),
        )
      )
    ),
    child.maybe <-- watch(state => state.elapsedMs > 0 || state.jobs.nonEmpty).map(show =>
      Option.when(show)(
        detailsTag(
          cls := "end-day",
          summaryTag("End day"),
          div(
            cls := "end-day__confirm",
            p("Cash and stock reset on the next loop. Unfinished jobs are lost."),
            button(
              tpe := "button",
              cls := "button button--quiet",
              "End the day",
              onClick --> (_ => day.update(_.endDay))
            ),
          ),
        )
      )
    ),
  )

  private def guide: HtmlElement = div(
    cls := "guide",
    div(
      p(child.text <-- watch(nextStep)),
      child.maybe <-- watch(state => state.loop == 1 && state.elapsedMs == 0 && state.jobs.isEmpty)
        .map(show => Option.when(show)(p(cls := "hint", "The clock runs only while you work."))),
    ),
    dayControls,
  )

  private def row(job: Job): HtmlElement = {
    val action = job match {
      case offer: Offer => s"Buy · ${offer.price} crowns"
      case _: Recipe    => "Make"
      case _: Order     => "Deliver"
    }
    val running = watch(_.jobs.find(_.id == job.id))
    val remaining = running
      .combineWith(watch(_.elapsedMs))
      .map { case (work, elapsed) =>
        work.map(active => seconds(active.endMs - elapsed))
      }
      .distinct
    val status = watch(state => if (state.paused) Some("Paused") else state.blocked(job))

    li(
      cls := "job-row",
      cls("job-row--running") <-- running.map(_.nonEmpty),
      div(
        cls := "job-copy",
        h3(job.title),
        job match {
          case offer: Offer =>
            div(
              cls := "job-meta",
              s"${offer.output.head.quantity} per batch · ",
              child.text <-- watch(state => s"${state.left(offer)} batches left")
            )
          case recipe: Recipe =>
            div(cls := "job-meta", amounts(recipe.input), span("→"), amounts(recipe.output))
          case order: Order =>
            div(
              div(
                cls := "job-meta",
                amounts(order.input),
                span("→"),
                strong(s"${order.reward} crowns"),
                span(
                  child.text <-- watch { state =>
                    val left = seconds(order.deadline * 1000 - state.elapsedMs)
                    if (state.delivered(order.id)) ""
                    else if (left == 0) "Closed"
                    else s"Due in ${left}s"
                  }
                )
              ),
              p(cls := "hint", order.milestone),
            )
        },
      ),
      div(
        cls := "job-action",
        button(
          tpe := "button",
          cls := "button button--primary",
          disabled <-- status.map(_.nonEmpty),
          aria.label := s"$action: ${job.title}",
          child.text <-- remaining.map(_.fold(action)(left => s"${left}s left")),
          onClick --> (_ => day.update(_.start(job))),
        ),
        span(
          cls := "job-status",
          child.text <-- running
            .combineWith(
              status,
              watch { state =>
                job match {
                  case _: Order => s"${seconds(state.duration(job))}s delivery"
                  case _        => s"${seconds(state.duration(job))}s"
                }
              }
            )
            .map {
              case (Some(_), Some("Paused"), _) => "Paused"
              case (Some(work), _, _)           => if (work.automatic) "Crew working" else "Working"
              case (_, blocked, regular)        => blocked.getOrElse(regular)
            }
        ),
      ),
      child.maybe <-- running
        .map(_.nonEmpty)
        .distinct
        .map(show =>
          Option.when(show)(
            div(
              cls := "job-progress",
              aria.hidden := true,
              div(styleAttr <-- running.combineWith(watch(_.elapsedMs)).map {
                case (work, elapsed) =>
                  val progress = work.fold(0)(active =>
                    100 * (elapsed - active.endMs + active.durationMs) / active.durationMs
                  )
                  s"width: ${progress.max(0).min(100)}%"
              })
            )
          )
        ),
    )
  }

  private def section(name: String, jobs: Vector[Job]): HtmlElement =
    div(
      cls := "station",
      h2(name),
      ul(jobs.map { job =>
        child.maybe <-- watch(_.has(job)).map(show => Option.when(show)(row(job)))
      })
    )

  private def controlSelect(caption: String, entries: Vector[(Int, String)], selected: Signal[Int])(
      update: (MarketDay, Int) => MarketDay
  ): HtmlElement =
    label(
      cls := "select-row",
      span(caption),
      select(
        aria.label := caption,
        entries.map { case (amount, text) => option(value := amount.toString, text) },
        value <-- selected.map(_.toString),
        onChange.mapToValue --> (raw =>
          raw.toIntOption.foreach(number => day.update(state => update(state, number)))
        ),
      )
    )

  private def stockTarget(job: Job): HtmlElement =
    controlSelect(
      s"Keep ${job.output.head.good.name.toLowerCase} in stock",
      Vector(0 -> "Off", 2 -> "2", 4 -> "4", 6 -> "6"),
      watch(_.targets.getOrElse(job.id, 0))
    )(_.setTarget(job, _))

  private def autoDelivery(order: Order): HtmlElement =
    label(
      cls := "select-row",
      span(order.title),
      input(
        tpe := "checkbox",
        checked <-- watch(_.autoOrders(order.id)),
        onChange --> (_ => day.update(_.toggleOrder(order)))
      )
    )

  private def automation: HtmlElement = detailsTag(
    cls := "automation",
    summaryTag(
      "Automation",
      span(
        cls := "automation-status",
        child.text <-- watch(state => if (state.planRunning) "On" else "Off")
      )
    ),
    div(
      cls := "automation-body",
      p(cls := "hint", "Set stock targets and choose which orders to send. Then run the plan."),
      div(
        cls := "automation-grid",
        div(
          h3("Buy"),
          Offers.map(offer =>
            child.maybe <-- watch(_.has(offer)).map(show => Option.when(show)(stockTarget(offer)))
          ),
          controlSelect(
            "Cash reserve",
            Vector(0 -> "None", 3 -> "3 crowns", 6 -> "6 crowns", 9 -> "9 crowns"),
            watch(_.reserve)
          )(_.setReserve(_)),
        ),
        child.maybe <-- watch(_.canAutomate(Workshop)).map(show =>
          Option.when(show)(
            div(
              h3("Make"),
              Recipes.map { recipe =>
                child.maybe <-- watch(_.has(recipe)).map(known =>
                  Option.when(known)(stockTarget(recipe))
                )
              }
            )
          )
        ),
        child.maybe <-- watch(_.canAutomate(Dispatch)).map(show =>
          Option.when(show)(
            div(
              h3("Deliver"),
              Orders.map { order =>
                child.maybe <-- watch(_.has(order)).map(known =>
                  Option.when(known)(autoDelivery(order))
                )
              }
            )
          )
        ),
      ),
      div(
        cls := "automation-run",
        button(
          tpe := "button",
          cls := "button button--primary",
          disabled <-- watch(state => !state.planRunning && !state.planConfigured),
          child.text <-- watch(state => if (state.planRunning) "Stop plan" else "Run plan"),
          onClick --> (_ => day.update(_.togglePlan)),
        ),
        child.maybe <-- watch(state =>
          Option.when(state.planRunning && state.jobs.isEmpty && !state.paused)(
            Lanes
              .flatMap(state.waitingFor)
              .distinct
              .headOption
              .getOrElse("Stock targets met and deliveries complete.")
          )
        ).map(_.map(message => span(cls := "hint", message))),
      ),
    ),
  )

  private def daySummary: HtmlElement = div(
    cls := "day-summary",
    h2(
      child.text <-- watch(state =>
        if (state.bothReliefOrders) "The town is ready" else "Day ended"
      )
    ),
    p(
      child.text <-- watch(state =>
        if (state.delivered.isEmpty) "No orders delivered."
        else s"Orders delivered: ${state.delivered.size}."
      )
    ),
    p("Trade unlocks, skills, and automation rules carry over."),
    button(
      tpe := "button",
      cls := "button button--primary",
      "Start the next loop",
      onClick --> (_ => day.update(_.nextLoop))
    ),
  )

  private def activity: HtmlElement = detailsTag(
    cls := "activity",
    summaryTag("Activity"),
    ul(
      children <-- watch(_.ledger.reverse.take(5))
        .map(_.map(entry => li(timeTag(clock(entry.atMs)), span(entry.text))))
    ),
  )

  private def work: HtmlElement = div(
    guide,
    child.maybe <-- watch(_.canAutomate(Supply)).map(show => Option.when(show)(automation)),
    section("Buy supplies", Offers),
    child.maybe <-- watch(workbenchVisible).map(show =>
      Option.when(show)(section("Workbench", Recipes))
    ),
    child.maybe <-- watch(ordersVisible).map(show => Option.when(show)(section("Orders", Orders))),
  )

  def view: HtmlElement = div(
    cls := "market-app",
    header,
    mainTag(
      child.maybe <-- saveWarning.map(show =>
        Option.when(show)(
          p(cls := "save-warning", "Could not save. Keep this tab open to continue.")
        )
      ),
      child <-- watch(_.ended).map(ended => if (ended) daySummary else work),
      child.maybe <-- watch(_.ledger.nonEmpty).map(show => Option.when(show)(activity)),
    ),
  )

}
