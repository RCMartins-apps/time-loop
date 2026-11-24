package pt.rcmartins.loop.model

import pt.rcmartins.loop.model.SkillState._
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

final case class SkillState(
    kind: ActionKind,
    loopLevel: Int, // 0..N
    loopXPMicro: Long, // current XP in loop level
    permLevel: Int, // 0..N
    permXPMicro: Long, // current XP in permanent level
    initialBonusMultiplier: Double,
    currentBonusMultiplier: Double,
) {

  def nextLoopXP: Long = LoopNextLevelXpCache(loopLevel)
  def nextLoopXPMicro: Long = nextLoopXP * 1_000_000L
  def loopRatio: Double = Math.min(1.0, loopXPMicro.toDouble / nextLoopXPMicro.toDouble)
  def loopXPLong: Long = loopXPMicro / 1_000_000L

  def nextPermXP: Long = PermanentNextLevelXpCache(permLevel)
  def nextPermXPMicro: Long = nextPermXP * 1_000_000L
  def permRatio: Double = Math.min(1.0, permXPMicro.toDouble / nextPermXPMicro.toDouble)
  def permXPLong: Long = permXPMicro / 1_000_000L

  def finalSpeedMulti: Double =
    (1.0 + loopLevel * LoopSpeedBonusPerLevel + permLevel * PermanentSpeedBonusPerLevel) *
      currentBonusMultiplier

  def resetLoopProgress: SkillState =
    copy(
      loopLevel = 0,
      loopXPMicro = 0L,
      currentBonusMultiplier = initialBonusMultiplier,
    )

}

object SkillState {

  def initial(kind: ActionKind): SkillState =
    SkillState(
      kind = kind,
      loopLevel = 0,
      loopXPMicro = 0,
      permLevel = 0,
      permXPMicro = 0,
      initialBonusMultiplier = 1.0,
      currentBonusMultiplier = 1.0,
    )

  private val NextLevelLoopXpFactor: Double = 1.1
  private val LoopSpeedBonusPerLevel: Double = 0.10

  private val NextLevelPermXpFactor: Double = 1.02
  private val PermanentSpeedBonusPerLevel: Double = 0.01

  private val LoopNextLevelXpCache: IndexedSeq[Long] =
    exponentialCalcLong(10, NextLevelLoopXpFactor)

  private val PermanentNextLevelXpCache: IndexedSeq[Long] =
    exponentialCalcLong(25, NextLevelPermXpFactor)

  private def exponentialCalcLong(base: Long, mult: Double): IndexedSeq[Long] = {
    var cache: IndexedSeq[Long] = Vector(base)
    var last: Double = base.toDouble
    for (_ <- 1 to 1000) {
      last *= mult
      cache = cache :+ Math.ceil(last).toLong
    }
    cache
  }

  implicit val decoder: JsonDecoder[SkillState] = DeriveJsonDecoder.gen[SkillState]
  implicit val encoder: JsonEncoder[SkillState] = DeriveJsonEncoder.gen[SkillState]

}
