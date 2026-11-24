package pt.rcmartins.loop.market

import com.raquo.laminar.api.L._
import pt.rcmartins.loop.market.Market._

final class MarketView(day: Var[MarketDay], saveWarning: Signal[Boolean]) {
  private def watch[A](f: MarketDay => A): Signal[A] = day.signal.map(f).distinct

  private def duration(ms: Int): String = s"${(ms + 999) / 1000}s"
  private def clock(ms: Int): String = {
    val seconds = (ms + 999) / 1000
    f"${seconds / 60}%02d:${seconds % 60}%02d"
  }

  private def stacks(items: Vector[Stack]): Seq[HtmlElement] = items.map { stack =>
    span(
      cls := "ingredient",
      span(aria.hidden := true, stack.good.icon),
      s"${stack.quantity} ${stack.good.name}"
    )
  }

  private def buttonText(text: String, style: String)(update: MarketDay => MarketDay): HtmlElement =
    button(tpe := "button", cls := style, text, onClick --> (_ => day.update(update)))

  private def notice: Signal[(String, String)] = watch { state =>
    if (state.fullRelief || state.bothReliefOrders)
      "The town is ready" -> "You lit the shelter and got Highbank across in the same loop. Keep refining your workshop plan whenever you return."
    else if (state.delivered(Highbank.id))
      "Highbank is safe" -> "The village made it across. Ring the bell to carry the rescue into the next loop."
    else if (state.milestones(Highbank.id) && !state.milestones(Merchant.id))
      "Prepare the whole town" -> "Light the market stalls to earn the guild’s advance and extra timber. That gives you enough to serve both the shelter and Highbank in a single loop."
    else if (state.milestones(Highbank.id))
      "Prepare the whole town" -> "Use the guild’s extra timber and run your crew in parallel. Deliver to the shelter and Highbank before their deadlines in the same loop."
    else if ((state.delivered -- state.milestones).nonEmpty)
      "Take the deal into tomorrow" -> "A new arrangement carries over at dawn. Use the remaining time for another order, or ring the bell to start the next loop."
    else if (state.milestones(Crossing.id))
      "Get Highbank across" -> s"Send provisions to Highbank before the flood. The bridge crew advances $BridgeAdvance crowns at dawn and lends you a courier."
    else if (state.milestones(Shelter.id))
      "Keep the low bridge open" -> "Buy rope and make two beam braces for the bridge crew. Assign Mara’s bench hand to craft while you manage supplies."
    else if (state.milestones.nonEmpty)
      "Light the hill shelter" -> "Send two lanterns to Mara. Your supplier can now restock while you work. Configure a target, then start your plan."
    else
      "Get two lanterns ready" -> "Buy timber and oil, make two lanterns, then choose a buyer. The guild pays more. Mara can lend you a bench hand."
  }

  private def currentStatus: Signal[String] = watch { state =>
    if (state.ended) "The bell has rung"
    else if (state.paused) "Paused"
    else if (state.jobs.nonEmpty) "Clock running"
    else if (state.planRunning) "Clock paused · plan waiting"
    else "Clock paused · start a job"
  }

  private def goal: HtmlElement =
    div(
      cls := "goal",
      span(cls := "goal__symbol", aria.hidden := true, "✦"),
      div(h2(child.text <-- notice.map(_._1)), p(child.text <-- notice.map(_._2))),
    )

  private def jobProgress(): HtmlElement = div(
    cls := "job-list",
    children <-- watch(_.jobs).split(_.id) { case (id, initial, job) =>
      div(
        cls := "job-progress",
        div(
          cls := "job-title",
          span(Jobs(id).title),
          span(cls := "job-owner", if (initial.automatic) "Crew" else "You")
        ),
        div(
          cls := "progress-track",
          aria.hidden := true,
          div(styleAttr <-- job.combineWith(watch(_.elapsedMs)).map { case (running, now) =>
            val pct = 100 * (running.durationMs - running.endMs + now) / running.durationMs
            s"width: ${pct.max(0).min(100)}%"
          })
        ),
        small(child.text <-- job.combineWith(watch(_.elapsedMs)).map { case (running, now) =>
          s"${duration((running.endMs - now).max(0))} left"
        }),
      )
    },
  )

  private def controlSelect(caption: String, entries: Vector[(Int, String)], selected: Signal[Int])(
      update: (MarketDay, Int) => MarketDay
  ): HtmlElement =
    label(
      cls := "select-row",
      span(caption),
      select(
        aria.label := caption,
        entries.map { case (amount, caption) => option(value := amount.toString, caption) },
        value <-- selected.map(_.toString),
        onChange.mapToValue --> (raw =>
          raw.toIntOption.foreach(amount => day.update(state => update(state, amount)))
        ),
      )
    )

  private def autoControl(job: Job): HtmlElement = job match {
    case order: Order =>
      button(
        tpe := "button",
        cls := "button button--quiet auto-order",
        aria.pressed <-- watch(_.autoOrders(order.id)).map(_.toString),
        child.text <-- watch(_.autoOrders(order.id)).map(on =>
          if (on) "Auto delivery on" else "Auto deliver"
        ),
        onClick --> (_ => day.update(_.toggleOrder(order)))
      )
    case _ =>
      controlSelect(
        s"Keep ${job.output.head.good.name.toLowerCase} in stock",
        Vector(0 -> "Off", 2 -> "2", 4 -> "4", 6 -> "6"),
        watch(_.targets.getOrElse(job.id, 0))
      )(_.setTarget(job, _))
  }

  private def card(job: Job): HtmlElement = {
    val action = job match {
      case offer: Offer => s"Buy · ${offer.price} crowns"
      case _: Recipe    => "Craft"
      case _: Order     => "Deliver"
    }
    val status = watch(_.blocked(job))
    val workRemaining = watch { state =>
      state.jobs.find(_.id == job.id).map(work => duration((work.endMs - state.elapsedMs).max(0)))
    }
    val working = workRemaining.map(_.nonEmpty).distinct
    val active = watch(state => !state.ended && !state.paused && state.blocked(job).isEmpty)
    val subtitle = job match {
      case offer: Offer => offer.description
      case order: Order => order.buyer
      case _: Recipe    => ""
    }
    div(
      cls := "job-card",
      cls("job-card--working") <-- working,
      div(
        cls := "job-card__head",
        div(h3(job.title), Option.when(subtitle.nonEmpty)(p(subtitle))),
        span(cls := "job-card__time", child.text <-- watch(state => duration(state.duration(job)))),
      ),
      if (job.input.nonEmpty) div(cls := "ingredients", stacks(job.input)) else emptyNode,
      job match {
        case order: Order =>
          div(
            cls := "job-card__terms",
            strong(s"${order.reward} crowns"),
            span(
              child.text <-- watch(state =>
                s"${clock((order.deadline * 1000 - state.elapsedMs).max(0))} to deadline"
              )
            )
          )
        case offer: Offer =>
          div(
            cls := "job-card__terms",
            div(cls := "ingredients", stacks(offer.output)),
            span(child.text <-- watch(state => s"${state.left(offer)} batches left"))
          )
        case _: Recipe => emptyNode
      },
      job match {
        case order: Order => p(cls := "order-reward", order.milestone)
        case _            => emptyNode
      },
      div(
        cls := "job-card__actions",
        button(
          tpe := "button",
          cls := "button button--primary",
          disabled <-- active.map(!_),
          aria.label := s"$action: ${job.title}",
          action,
          onClick --> (_ => day.update(_.start(job)))
        ),
        span(
          cls := "job-status",
          child.text <-- workRemaining.combineWith(status).map {
            case (Some(left), _) => s"In progress · $left left"
            case (_, blocked)    => blocked.getOrElse("")
          }
        ),
      ),
      child.maybe <-- watch(_.canAutomate(job.lane)).map(unlocked =>
        Option.when(unlocked)(autoControl(job))
      ),
    )
  }

  private def section(lane: Lane, title: String, jobs: Vector[Job]): HtmlElement =
    div(
      cls := "station",
      idAttr := lane.id,
      div(
        cls := "station__head",
        h2(title),
        span(
          cls := "station__skill",
          child.text <-- watch(state => f"${state.speed(lane)}%.2f× speed")
        )
      ),
      div(
        cls := "card-grid",
        jobs.map(job => child.maybe <-- watch(_.has(job)).map(show => Option.when(show)(card(job))))
      ),
    )

  private def floodSummary: HtmlElement =
    div(
      cls := "day-summary",
      h2(
        child.text <-- watch(state =>
          if (state.delivered(Highbank.id)) "Everyone made it across" else "The river rises"
        )
      ),
      p(child.text <-- watch { state =>
        if (state.delivered.isEmpty)
          "No orders delivered today. Try buying supplies and making two lanterns tomorrow."
        else
          s"You delivered ${state.delivered.size} ${if (state.delivered.size == 1) "order"
            else "orders"} and finished with ${state.crowns} crowns."
      }),
      p(
        "The workshop resets at dawn. Route knowledge, skill progress, and automation rules carry over."
      ),
      buttonText("Start the next loop", "button button--primary")(_.nextLoop),
    )

  private def planControls: HtmlElement = div(
    cls := "plan",
    h3("Workshop plan"),
    controlSelect(
      "Keep a cash reserve",
      Vector(0 -> "None", 3 -> "3 crowns", 6 -> "6 crowns", 9 -> "9 crowns"),
      watch(_.reserve)
    )((state, amount) => state.setReserve(amount)),
    button(
      tpe := "button",
      cls := "button button--primary",
      disabled <-- watch(state => state.ended || (!state.planRunning && !state.planConfigured)),
      child.text <-- watch(state => if (state.planRunning) "Stop plan" else "Run plan"),
      onClick --> (_ => day.update(_.togglePlan)),
    ),
    child.maybe <-- watch(state =>
      Option.when(state.planRunning && state.jobs.isEmpty)(
        Lanes
          .flatMap(lane => state.waitingFor(lane))
          .distinct
          .headOption
          .getOrElse("Stock targets met and deliveries complete.")
      )
    ).map(_.map(message => p(cls := "plan-status", message))),
    child.maybe <-- watch(state => !state.planConfigured).map(empty =>
      Option.when(empty)(
        p(cls := "plan-status", "Set a stock target on a supply card to get started.")
      )
    ),
  )

  private def dayPanel: HtmlElement = div(
    cls := "panel day-panel",
    idAttr := "market-clock",
    div(
      cls := "day-panel__head",
      h2("Market clock"),
      span(child.text <-- watch(s => s"Loop ${s.loop}"))
    ),
    div(cls := "day-clock", child.text <-- watch(s => clock(s.timeLeftMs))),
    div(
      cls := "progress-track progress-track--clock",
      aria.hidden := true,
      div(styleAttr <-- watch(s => s"width: ${100 * s.timeLeftMs / (DaySeconds * 1000)}%"))
    ),
    p(cls := "clock-status", child.text <-- currentStatus),
    jobProgress(),
    child.maybe <-- watch(state => state.canAutomate(Supply) && !state.ended).map(show =>
      Option.when(show)(planControls)
    ),
    child.maybe <-- watch(state => !state.ended && (state.elapsedMs > 0 || state.jobs.nonEmpty))
      .map(show =>
        Option.when(show)(
          div(
            cls := "day-controls",
            button(
              tpe := "button",
              cls := "button button--quiet",
              child.text <-- watch(state => if (state.paused) "Resume" else "Pause"),
              onClick --> (_ => day.update(state => state.copy(paused = !state.paused))),
            ),
            detailsTag(
              cls := "bell-control",
              summaryTag("Ring the bell"),
              p("End this day. Work still in progress will be lost."),
              buttonText("End the day", "button button--quiet")(_.endDay),
            ),
          )
        )
      ),
  )

  private def stockPanel: HtmlElement = div(
    cls := "panel stock-panel",
    div(
      cls := "section-head",
      h2("Stockroom"),
      strong(cls := "crowns", child.text <-- watch(state => s"${state.crowns} crowns"))
    ),
    ul(Goods.map { good =>
      child.maybe <-- watch(state => good != Rope && good != Brace || state.milestones(Shelter.id))
        .map(show =>
          Option.when(show)(
            li(
              span(span(aria.hidden := true, good.icon), good.name),
              strong(child.text <-- watch(state => state.count(good).toString))
            )
          )
        )
    }),
  )

  private def ledger: HtmlElement = div(
    cls := "panel ledger",
    h2("Daybook"),
    children <-- watch(_.ledger.reverse.take(5)).map { entries =>
      if (entries.isEmpty) Seq(p("The workshop opens at dawn."))
      else entries.map(entry => p(timeTag(clock(entry.atMs)), span(entry.text)))
    },
  )

  private def header: HtmlElement = headerTag(
    cls := "topbar",
    div(
      cls := "topbar__main",
      span(cls := "brand", span(cls := "brand-mark", aria.hidden := true, "◴"), "TIME LOOP"),
      span(
        cls := "top-stock",
        child.text <-- watch(s => s"${s.crowns} crowns · ${clock(s.timeLeftMs)}")
      ),
      detailsTag(
        cls := "settings",
        summaryTag("Settings"),
        div(
          cls := "settings-menu",
          button(
            tpe := "button",
            cls := "button button--quiet",
            dataAttr("install-action") := "install",
            "Install game"
          ),
          p("Progress is saved on this device. The clock pauses when you leave."),
        )
      ),
    ),
    navTag(
      cls := "station-links",
      aria.label := "Workshop shortcuts",
      a(href := "#market-clock", "Clock"),
      a(href := "#supply", "Buy"),
      a(href := "#workshop", "Craft"),
      a(href := "#dispatch", "Orders"),
    ),
  )

  def view: HtmlElement = div(
    cls := "market-app",
    header,
    mainTag(
      div(
        cls := "hero",
        div(
          cls := "hero__copy",
          p(cls := "hero__label", "A WORKSHOP AT THE WATER’S EDGE"),
          h1("The Last Market Day"),
          p(
            "The river rises at the evening bell. Dawn brings the town back, and you remember what worked."
          )
        ),
      ),
      child.maybe <-- saveWarning.map(failed =>
        Option.when(failed)(
          p(
            cls := "save-warning",
            "This browser could not store your progress. Keep this tab open to continue."
          )
        )
      ),
      goal,
      child.maybe <-- watch(_.ended).map(ended => Option.when(ended)(floodSummary)),
      div(
        cls := "dashboard",
        div(cls := "status-column", dayPanel, stockPanel, ledger),
        div(
          cls := "production-column",
          section(Supply, "Suppliers", Offers),
          section(Workshop, "Workbench", Recipes),
          section(Dispatch, "Orders", Orders),
        ),
      ),
    ),
  )

}
