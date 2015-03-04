package unitCalculator

import collection.Map
/**
 * Created by Gustavo on 9/19/14.
 */

object Dimension {
  def apply(mass: Int, length : Int, time: Int, current: Int, temperature : Int, luminance: Int) : Dimension = {
    Dimension(Map[SIUnit, Int](
      SIUnit.kg -> mass,
      SIUnit.m -> length,
      SIUnit.s -> time,
      SIUnit.A -> current,
      SIUnit.K -> temperature,
      SIUnit.cd -> luminance))
  }
}

case class Dimension(units: Map[SIUnit, Int]) {

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Dimension]

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that : Dimension => this.canEqual(that) && units == that.units
      case _ => false
    }
  }

  private def operate(change: (Int,Int) => Int, unitMap : Map[SIUnit,Int]): Map[SIUnit, Int] = {
    units ++ unitMap.map{ case (k,v) => k -> change(units.getOrElse(k,0),v) }
  }

  def *(other : Dimension) : Dimension = {
    Dimension(operate(_+_, other.units))
  }

  def /(other : Dimension) : Dimension = {
    Dimension(operate(_-_, other.units))
  }

  override def toString: String = {
    units
      .collect{ case (unit, power) if power != 0 =>
        unit + (if(power != 1) { "^" + power } else "")
    }.mkString(" ")
  }
}
