package com.softwaremill.sgap

import org.jgap.impl._
import org.{jgap => j}

abstract class GeneticOperator[A: Chromosome: EvolutionRun] extends j.GeneticOperator {

  def apply(chromos: Seq[A]): Seq[A]

}

object GeneticOperator {

  object mutation {

    private implicit class RateAsInt(val rate: Double) extends AnyVal {
      def rateAsInt: Int = (1 / rate).toInt
    }

    def default(rate: Double)(implicit c: EvolutionRun[_]) = new MutationOperator(c.jConfig, rate.rateAsInt)

    def gaussian(rate: Double = 0.05d)(implicit c: EvolutionRun[_]) = new GaussianMutationOperator(c.jConfig, rate)

    def inverting()(implicit c: EvolutionRun[_]) = new InversionOperator(c.jConfig)

    def swapping(rate: Double)(implicit c: EvolutionRun[_]) = new SwappingMutationOperator(c.jConfig, rate.rateAsInt)

    def swapping(rate: Double, range: Int)(implicit c: EvolutionRun[_]) =
      new RangedSwappingMutationOperator(c.jConfig, rate.rateAsInt, range)

    def twoWay(rate: Double)(implicit c: EvolutionRun[_]) = new TwoWayMutationOperator(c.jConfig, rate.rateAsInt)
  }

  object crossover {

    def standard(rate: Double, allowNew: Boolean = false)(implicit c: EvolutionRun[_]) =
      new CrossoverOperator(c.jConfig, rate, false, allowNew)

    def full(rate: Double, allowNew: Boolean = false)(implicit c: EvolutionRun[_]) =
      new CrossoverOperator(c.jConfig, rate, true, allowNew)

    def averaging()(implicit c: EvolutionRun[_]) = new AveragingCrossoverOperator(c.jConfig)

    def greedy()(implicit c: EvolutionRun[_]) = new GreedyCrossover(c.jConfig)

  }

}
