package com.softwaremill.helisa

import org.jgap.IChromosome
import org.jgap.{FitnessFunction => JFitness}

object FitnessFunction {
  def apply(f: IChromosome => Double) = new JFitness {
    def evaluate(iChromosome: IChromosome): Double = f(iChromosome)
  }
}
