package com.softwaremill.sgap

import org.{jgap => j}
class Configuration[A: Chromosome] private (fitnessFunction: (A => Double)) {

  private[sgap] val jConfig: j.Configuration = new j.impl.DefaultConfiguration

  {
    jConfig.setFitnessFunction((jChromo: j.IChromosome) => jChromo.fromJ.map(fitnessFunction).getOrElse(0))
  }

  def populationSize_=(size: Int): Unit = jConfig.setPopulationSize(size)
  def populationSize: Int               = jConfig.getPopulationSize

  def sampleChromosome: A =
    jConfig.getSampleChromosome.fromJ
      .fold(_ => throw new IllegalStateException("Cannot convert sample chromosome, check your implicits"), identity)
  def sampleChromosome_=(sampleChromosome: A): Unit = jConfig.setSampleChromosome(sampleChromosome.toJ(this))

}

object Configuration {

  def apply[A: Chromosome](fitnessFunction: (A => Double)): Configuration[A] = new Configuration[A](fitnessFunction)

}
