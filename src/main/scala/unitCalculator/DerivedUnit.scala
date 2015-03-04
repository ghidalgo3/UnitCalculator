package unitCalculator

sealed trait DerivedUnit {
  def dimen : Dimension
}

object DerivedUnit {
  case object Newton extends DerivedUnit {
    val dimen = Dimension(Map(SIUnit.kg -> 1, SIUnit.m -> 1, SIUnit.s -> -2))

    override def toString: String = "N"
  }

  case object Couloumb extends DerivedUnit {
    val dimen = Dimension(Map(SIUnit.A -> 1, SIUnit.s -> 1))
    override def toString: String = "C"
  }

  case object Joule extends DerivedUnit {
    val dimen = Dimension(Map(SIUnit.kg -> 1, SIUnit.m -> 2, SIUnit.s -> -2))
    override def toString: String = "J"
  }

  case object Watt extends DerivedUnit {
    val dimen = Dimension(Map(SIUnit.kg -> 1, SIUnit.m -> 2, SIUnit.s -> -3))
    override def toString: String = "W"
  }

}
