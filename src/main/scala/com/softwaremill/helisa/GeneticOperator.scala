package com.softwaremill.helisa

import org.jgap.impl._
import org.{jgap => j}

abstract class GeneticOperator[A: Chromosome: Evololver] extends j.GeneticOperator {

  def apply(chromos: Seq[A]): Seq[A]

}

object GeneticOperator {

  object mutation {

    private implicit class RateAsInt(val rate: Double) extends AnyVal {
      def rateAsInt: Int = (1 / rate).toInt
    }

    def default(rate: Double)(implicit c: Evololver[_]) = new MutationOperator(c.jConfig, rate.rateAsInt)

    def gaussian(rate: Double = 0.05d)(implicit c: Evololver[_]) = new GaussianMutationOperator(c.jConfig, rate)

    def inverting()(implicit c: Evololver[_]) = new InversionOperator(c.jConfig)

    def swapping(rate: Double)(implicit c: Evololver[_]) = new SwappingMutationOperator(c.jConfig, rate.rateAsInt)

    def swapping(rate: Double, range: Int)(implicit c: Evololver[_]) =
      new RangedSwappingMutationOperator(c.jConfig, rate.rateAsInt, range)

    def twoWay(rate: Double)(implicit c: Evololver[_]) = new TwoWayMutationOperator(c.jConfig, rate.rateAsInt)
  }

  object crossover {

    def standard(rate: Double, allowNew: Boolean = false)(implicit c: Evololver[_]) =
      new CrossoverOperator(c.jConfig, rate, false, allowNew)

    def full(rate: Double, allowNew: Boolean = false)(implicit c: Evololver[_]) =
      new CrossoverOperator(c.jConfig, rate, true, allowNew)

    def averaging()(implicit c: Evololver[_]) = new AveragingCrossoverOperator(c.jConfig)

    def greedy()(implicit c: Evololver[_]) = new GreedyCrossover(c.jConfig)

  }

}
