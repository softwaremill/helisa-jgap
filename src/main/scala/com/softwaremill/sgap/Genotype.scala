package com.softwaremill.sgap

import org.{jgap => j}

class Genotype[A: Chromosome] private (configuration: Configuration[A]) {

  private[sgap] val jGenotype = j.Genotype.randomInitialGenotype(configuration.jConfig)

  def evolve(numberOfEvolutions: Int = 1): Genotype[A] = {
    jGenotype.evolve(numberOfEvolutions)
    this
  }

  def fittestChromosome: Option[A] = jGenotype.getFittestChromosome.fromJ.toOption

  def fitnessValue(chromosome: A): Double = chromosome.toJ(configuration).getFitnessValue

}

object Genotype {

  def randomGenotype[A: Chromosome](configuration: Configuration[A]): Genotype[A] = new Genotype[A](configuration)

}
