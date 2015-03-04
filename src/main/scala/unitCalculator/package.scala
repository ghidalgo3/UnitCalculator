package object unitCalculator {

  trait commonPimp extends Any {

    def toDouble : Double

    def kg = Scalar(toDouble, SIUnit.kg)
    def kgs: Scalar = kg
    def C = Scalar(toDouble, DerivedUnit.Couloumb.dimen)
    def s = Scalar(toDouble, SIUnit.s)
    def m = Scalar(toDouble, SIUnit.m)
    def N = Scalar(toDouble, DerivedUnit.Newton.dimen)
    def lb = Scalar(toDouble / 2.204 , SIUnit.kg)//pounds / kilogram
    def lbs = lb
  }

  implicit class unit_PimpMyDouble(val self: Double) extends AnyVal with commonPimp {
    override def toDouble: Double = self
  }

  implicit class unit_PimpMyInt(val self: Int) extends AnyVal with commonPimp {
    override def toDouble: Double = self
  }
}
