package pt.rcmartins.loop.model

import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class UIDiscovery(
    inventory: Boolean = false,
    map: Boolean = false,
    story: Boolean = false,
    skills: Boolean = false,
) {
  def discover(state: GameState): UIDiscovery = copy(
    inventory = inventory || state.inventory.items.exists(_._2 > 0),
    map = map || state.visibleMoveActions.exists(_.data.moveToArea.nonEmpty),
    story = story || state.storyActionsHistory.nonEmpty,
    skills = skills || state.skills.allHigherThan0.nonEmpty,
  )

}

object UIDiscovery {
  implicit val decoder: JsonDecoder[UIDiscovery] = DeriveJsonDecoder.gen[UIDiscovery]
  implicit val encoder: JsonEncoder[UIDiscovery] = DeriveJsonEncoder.gen[UIDiscovery]
}
