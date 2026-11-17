package pt.rcmartins.loop

import pt.rcmartins.loop.data.StoryActions
import pt.rcmartins.loop.model._
import pt.rcmartins.loop.model.saved.GameStateSaved
import zio.Scope
import zio.json._
import zio.test.{assertTrue, Spec, ZIOSpecDefault}

object UIDiscoveryTest extends ZIOSpecDefault {
  private def initial = GameState.initial(0L)

  override def spec: Spec[Scope, Any] = suite("UI discovery")(
    test("a new game hides all optional sections") {
      assertTrue(initial.discoveredUI == UIDiscovery())
    },
    test("each section unlocks from its own content") {
      val inventory = initial.copy(inventory = initial.inventory.addItem(ItemType.Coins, 1))
      val story = initial.copy(storyActionsHistory = Seq(StoryLineHistory("First story")))
      val skills = initial.copy(skills = initial.skills.copy(agility = initial.skills.agility.copy(permXPMicro = 1)))
      val destination = StoryActions.Data.WakeUp.copy(moveToArea = Some(CharacterArea.Area1_Home)).toActiveAction
      val map = initial.copy(visibleMoveActions = Seq(destination))
      assertTrue(
        inventory.discoveredUI == UIDiscovery(inventory = true),
        story.discoveredUI == UIDiscovery(story = true),
        skills.discoveredUI == UIDiscovery(skills = true),
        map.discoveredUI == UIDiscovery(map = true),
      )
    },
    test("discovery survives empty inventory, new loops, and saved game round trips") {
      val discovered = UIDiscovery(inventory = true, map = true, story = true, skills = true)
      val state = initial.copy(uiDiscovery = discovered)
      val saved = GameStateSaved.fromGameState(state.resetForNewLoop)
      val restored = saved.toJson.fromJson[GameStateSaved].map(_.toGameState(0L))
      assertTrue(
        state.discoveredUI == discovered,
        state.resetForNewLoop.discoveredUI == discovered,
        restored.exists(_.discoveredUI == discovered),
      )
    },
    test("older saves without discovery metadata still load and infer current content") {
      val state = initial.copy(inventory = initial.inventory.addItem(ItemType.Coins, 1))
      val json = GameStateSaved.fromGameState(state).toJson
      val withoutDiscovery = json.take(json.lastIndexOf(",\"uiDiscovery\"")) + "}"
      val restored = withoutDiscovery.fromJson[GameStateSaved].map(_.toGameState(0L))
      assertTrue(restored.exists(_.discoveredUI.inventory))
    },
    test("a leveled skill stays visible when its current XP is zero") {
      val state = initial.copy(skills = initial.skills.copy(agility = initial.skills.agility.copy(permLevel = 1)))
      assertTrue(state.discoveredUI.skills, state.skills.allHigherThan0.nonEmpty)
    },
  )
}
