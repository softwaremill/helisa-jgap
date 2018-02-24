package com.softwaremill.sgap.gene

import com.softwaremill.sgap.Configuration
import org.{jgap => j}

trait Gene[Value, Underlying <: j.Gene] {

  def jGene: Underlying
  def value: Value

}

case class MultipleIntGene(jGene: j.impl.MutipleIntegerGene = new j.impl.MutipleIntegerGene())
    extends Gene[Int, j.impl.MutipleIntegerGene] {

  def value = jGene.getAllele.asInstanceOf[Integer]

}

object MultipleIntGene {

  def apply(lowerBound: Int, upperBound: Int, multiple: Int)(implicit configuration: Configuration[_]): MultipleIntGene =
    apply(new j.impl.MutipleIntegerGene(configuration.jConfig, lowerBound, upperBound, multiple))

}

case class IntGene(jGene: j.impl.IntegerGene = new j.impl.IntegerGene()) extends Gene[Int, j.impl.IntegerGene] {

  def value = jGene.getAllele.asInstanceOf[Integer]

  val copy = (u: j.impl.IntegerGene) => new IntGene(u)
}

object IntGene {

  def apply(lowerBound: Int, upperBound: Int)(implicit configuration: Configuration[_]): IntGene =
    apply(new j.impl.IntegerGene(configuration.jConfig, lowerBound, upperBound))

}

case class BooleanGene(jGene: j.impl.BooleanGene = new j.impl.BooleanGene()) extends Gene[Boolean, j.impl.BooleanGene] {

  def this() = this(new j.impl.BooleanGene())

  def value = jGene.getAllele.asInstanceOf[java.lang.Boolean]

}

object BooleanGene {

  def apply(startValue: Boolean)(implicit configuration: Configuration[_]): BooleanGene =
    apply(new j.impl.BooleanGene(configuration.jConfig, startValue))

}
