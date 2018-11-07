package com.softwaremill.helisa.api

import com.softwaremill.helisa._
import org.jgap.impl._
import org.{jgap => j}

abstract class GeneticOperator[G: Genotype: EvolverConfig] extends j.GeneticOperator {

  def apply(chromos: Seq[G]): Seq[G]

}

object GeneticOperator {

  object mutation {

    private implicit class RateAsInt(val rate: Double) extends AnyVal {
      def rateAsInt: Int = (1 / rate).toInt
    }

    def default(rate: Double = 0.083d)(implicit c: EvolverConfig[_]) = new MutationOperator(c.jConfig, rate.rateAsInt)

    def gaussian(rate: Double = 0.05d)(implicit c: EvolverConfig[_]) = new GaussianMutationOperator(c.jConfig, rate)

    def inverting()(implicit c: EvolverConfig[_]) = new InversionOperator(c.jConfig)

    def swapping(rate: Double)(implicit c: EvolverConfig[_]) = new SwappingMutationOperator(c.jConfig, rate.rateAsInt)

    def swapping(rate: Double, range: Int)(implicit c: EvolverConfig[_]) =
      new RangedSwappingMutationOperator(c.jConfig, rate.rateAsInt, range)

    def twoWay(rate: Double)(implicit c: EvolverConfig[_]) = new TwoWayMutationOperator(c.jConfig, rate.rateAsInt)
  }

  object crossover {

    def standard(rate: Double = 0.35d, allowNew: Boolean = false)(implicit c: EvolverConfig[_]) =
      new CrossoverOperator(c.jConfig, rate, false, allowNew)

    def full(rate: Double, allowNew: Boolean = false)(implicit c: EvolverConfig[_]) =
      new CrossoverOperator(c.jConfig, rate, true, allowNew)

    def averaging()(implicit c: EvolverConfig[_]) = new AveragingCrossoverOperator(c.jConfig)

    def greedy()(implicit c: EvolverConfig[_]) = new GreedyCrossover(c.jConfig)

  }

}
