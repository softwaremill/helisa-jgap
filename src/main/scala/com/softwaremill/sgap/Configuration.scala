package com.softwaremill.sgap

import org.{jgap => j}
class Configuration[A: Chromosome] private (fitnessFunction: (A => Double)) {

  private[sgap] val jConfig: j.Configuration = new j.impl.DefaultConfiguration

  {
    jConfig.setFitnessFunction((jChromo: j.IChromosome) => jChromo.fromJ.map(fitnessFunction).getOrElse(0))
  }


  def sampleChromosome: A =
  jConfig.getSampleChromosome.fromJ
    .fold(_ => throw new IllegalStateException("Cannot convert sample chromosome, check your implicits"), identity)
  def sampleChromosome_=(sampleChromosome: A): Unit = jConfig.setSampleChromosome(sampleChromosome.toJ(this))

  def maxPopulationSize_=(size: Int): Unit = jConfig.setPopulationSize(size)
  def maxPopulationSize: Int               = jConfig.getPopulationSize

  def minPopulationSizeRatio_=(ratio: Double): Unit = jConfig.setMinimumPopSizePercent((ratio*100).toInt)
  def minPopulationSizeRatio: Double = jConfig.getMinimumPopSizePercent/100.0

  def randomGenerator: j.RandomGenerator = jConfig.getRandomGenerator
  def randomGenerator_=(g: j.RandomGenerator): Unit = jConfig.setRandomGenerator(g)

  //TODO: what about removing? Add another layer of proxy?
  def addNaturalSelector(selector: NaturalSelector[A, _], beforeGeneticOperators: Boolean): Unit = jConfig.addNaturalSelector(selector, beforeGeneticOperators)


}

object Configuration {

  def apply[A: Chromosome](fitnessFunction: (A => Double)): Configuration[A] = new Configuration[A](fitnessFunction)

}
