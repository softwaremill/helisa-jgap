package com.softwaremill.sgap

import org.{jgap => j}
import scala.collection.JavaConverters._
class EvolutionRun[A: Chromosome] private(fitnessFunction: A => Double) {

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

  private var validatorActual: Option[ChromosomeValidator[A]] = None
  def validator: Option[ChromosomeValidator[A]] = validatorActual
  def validator_=(v: ChromosomeValidator[A]): Unit = {
    validatorActual = Some(v)
  }

  lazy val naturalSelectorsPreGeneticOperators: ConfigurationParameters[j.NaturalSelector] = new NaturalSelectorConfigurationParameters(jConfig, isPre = true)

  lazy val naturalSelectorsPostGeneticOperators: ConfigurationParameters[j.NaturalSelector] = new NaturalSelectorConfigurationParameters(jConfig, isPre = false)

  lazy val geneticOperators: ConfigurationParameters[j.GeneticOperator] = new ConfigurationParameters[j.GeneticOperator] {
    def add(newValue: j.GeneticOperator): Unit = jConfig.addGeneticOperator(newValue)

    def remove(toRemove: j.GeneticOperator): Unit = {
      if(jConfig.isLocked) {
        throw new UnsupportedOperationException("Cannot remove operator out of a configuration in use, please create a new Configuration!")
      }
      jConfig.getGeneticOperators.remove(toRemove)
    }

    def get(): Seq[j.GeneticOperator] = jConfig.getGeneticOperators.asInstanceOf[java.util.List[j.GeneticOperator]].asScala.view.toSeq

    def size: Int = jConfig.getGeneticOperators.size()
  }

}

object EvolutionRun {

  def apply[A: Chromosome](fitnessFunction: A => Double): EvolutionRun[A] = new EvolutionRun[A](fitnessFunction)

}

abstract class ConfigurationParameters[Param] private[sgap] () {

  def get(): Seq[Param]

  def add(newValue: Param): Unit

  def remove(toRemove: Param): Unit

  def size: Int

}

private class NaturalSelectorConfigurationParameters(jConfig: j.Configuration, isPre: Boolean) extends ConfigurationParameters[j.NaturalSelector] {
  def get(): Seq[j.NaturalSelector] = jConfig.getNaturalSelectors(isPre).iterator().asInstanceOf[java.util.Iterator[j.NaturalSelector]].asScala.toSeq

  def add(newValue: j.NaturalSelector): Unit = jConfig.addNaturalSelector(newValue, isPre)

  def remove(toRemove: j.NaturalSelector): Unit = jConfig.removeNaturalSelectors(isPre)

  def size: Int = jConfig.getNaturalSelectorsSize(isPre)
}