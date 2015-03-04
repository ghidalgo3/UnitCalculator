package unitCalculator


sealed trait SIUnit

object SIUnit {

  case object kg  extends SIUnit
  case object m   extends SIUnit
  case object s   extends SIUnit
  case object K   extends SIUnit
  case object A   extends SIUnit
  case object mol extends SIUnit
  case object cd  extends SIUnit

  val values = Set(kg,m,s,K,A,mol,cd)
}
