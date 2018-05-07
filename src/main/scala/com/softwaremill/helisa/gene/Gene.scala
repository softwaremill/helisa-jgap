package com.softwaremill.helisa.gene

import com.softwaremill.helisa._
import org.{jgap => j}

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.JavaConverters._

trait Gene[Value, Underlying <: j.Gene] {

  def jGene: Underlying
  def value: Value = jGene.getAllele.asInstanceOf[Value]

  override def toString: String = jGene.toString

}

class BooleanGene(val jGene: j.impl.BooleanGene) extends Gene[Boolean, j.impl.BooleanGene] {
  override def value: Boolean = jGene.getAllele.asInstanceOf[java.lang.Boolean]
}

class IntOfMultipleGene(val jGene: j.impl.MutipleIntegerGene) extends Gene[Int, j.impl.MutipleIntegerGene] {
  override def value = jGene.getAllele.asInstanceOf[Integer]
}

class IntGene(val jGene: j.impl.IntegerGene) extends Gene[Int, j.impl.IntegerGene] {
  override def value = jGene.getAllele.asInstanceOf[Integer]
}

class DoubleGene(val jGene: j.impl.DoubleGene) extends Gene[Double, j.impl.DoubleGene] {
  override def value = jGene.getAllele.asInstanceOf[Double]
}

class StringGene(val jGene: j.impl.StringGene) extends Gene[String, j.impl.StringGene] {
}

class BitGene(val jGene: j.impl.FixedBinaryGene) extends Gene[Array[Int], j.impl.FixedBinaryGene] {
}

class DiscreteValueGene[+V](val jGene: j.impl.MapGene) extends Gene[Map[String, V @uncheckedVariance], j.impl.MapGene] {
  override def value = jGene.getAllele.asInstanceOf[Map[String, V]]
}

object Gene {

  type JGeneResolver = PartialFunction[j.Gene, Gene[_, _]]

  def fromJ[V](jGene: j.Gene)(implicit resolver: JGeneResolver): Gene[V, _] = resolver(jGene).asInstanceOf[Gene[V, _]]

  object genes {

    def boolean(value: Boolean = false)(implicit c: EvolutionRun[_]) =
      new BooleanGene(new j.impl.BooleanGene(c.jConfig, value))

    def int(min: Int, max: Int)(implicit c: EvolutionRun[_]) =
      new IntGene(new j.impl.IntegerGene(c.jConfig, min, max))

    def intOfMultiple(min: Int, max: Int, multiple: Int)(implicit c: EvolutionRun[_]) =
      new IntOfMultipleGene(new j.impl.MutipleIntegerGene(c.jConfig, min, max, multiple))

    def double(min: Double, max: Double)(implicit c: EvolutionRun[_]) =
      new DoubleGene(new j.impl.DoubleGene(c.jConfig, min, max))

    def string(alphabet: List[Char], minLength: Int, maxLenght: Int)(implicit c: EvolutionRun[_]) =
      new StringGene(new j.impl.StringGene(c.jConfig, minLength, maxLenght, alphabet.mkString))

    def bit(minLength: Int, maxLength: Int)(implicit c: EvolutionRun[_]) =
      new IntGene(new j.impl.IntegerGene(c.jConfig, minLength, maxLength))

    def discreteValues[V](valueMap: Map[String, V])(implicit c: EvolutionRun[_]) =
      new DiscreteValueGene[V](new j.impl.MapGene(c.jConfig, valueMap.asJava))

  }

}
