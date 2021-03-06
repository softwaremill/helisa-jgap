package com.softwaremill.helisa

import com.softwaremill.helisa.api.{Genotype, GenotypeValidator}
import org.{jgap => j}

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
class EvolverConfig[G: Genotype] private (fitnessFunction: G => Double) {

  private[helisa] val jConfig: j.Configuration = {
    val c = new j.impl.DefaultConfiguration()
    j.Configuration.reset() //TODO: is this actually thread-safe for multiple runs?
    c
  }

  private val rawFitnessFunctions: j.IChromosome => Double = (jChromo: j.IChromosome) =>
    jChromo.fromJ.map(fitnessFunction).getOrElse(0)

  private val ec = ExecutionContext.global

  private def fitnessFunctionThroughEc = (jChromo: j.IChromosome) => {
    val calculation = Future { rawFitnessFunctions(jChromo) }(ec)
    Await.result(calculation, Duration.Inf)
  }

  {
    jConfig.setFitnessFunction(fitnessFunctionThroughEc(_))
  }

  def sampleGenotype: G =
    jConfig.getSampleChromosome.fromJ
      .fold(_ => throw new IllegalStateException("Cannot convert sample genotype, check your implicits"), identity)
  def sampleGenotype_=(sampleGenotype: G): Unit = jConfig.setSampleChromosome(sampleGenotype.toJ(this))

  def maxPopulationSize_=(size: Int): Unit = jConfig.setPopulationSize(size)
  def maxPopulationSize: Int               = jConfig.getPopulationSize

  def minPopulationSizeRatio_=(ratio: Double): Unit = jConfig.setMinimumPopSizePercent((ratio * 100).toInt)
  def minPopulationSizeRatio: Double                = jConfig.getMinimumPopSizePercent / 100.0

  def randomGenerator: j.RandomGenerator            = jConfig.getRandomGenerator
  def randomGenerator_=(g: j.RandomGenerator): Unit = jConfig.setRandomGenerator(g)

  private var validatorActual: Option[GenotypeValidator[G]] = None
  def validator: Option[GenotypeValidator[G]]               = validatorActual
  def validator_=(v: GenotypeValidator[G]): Unit =
    validatorActual = Some(v)

  lazy val naturalSelectorsPre: ConfigurationParameters[j.NaturalSelector] with ParametersBulkRemove =
    new NaturalSelectorConfigurationParameters(jConfig, isPre = true)

  lazy val naturalSelectorsPost: ConfigurationParameters[j.NaturalSelector] with ParametersBulkRemove =
    new NaturalSelectorConfigurationParameters(jConfig, isPre = false)

  lazy val geneticOperators: ConfigurationParameters[j.GeneticOperator] with ParametersSelectiveRemove[j.GeneticOperator] =
    new ConfigurationParameters[j.GeneticOperator] with ParametersSelectiveRemove[j.GeneticOperator] {
      def add(newValue: j.GeneticOperator): Unit = jConfig.addGeneticOperator(newValue)

      def remove(toRemove: j.GeneticOperator): Unit = {
        if (jConfig.isLocked) {
          throw new UnsupportedOperationException(
            "Cannot remove operator out of a configuration in use, please create a new Configuration!")
        }
        jConfig.getGeneticOperators.remove(toRemove)
      }

      def get(): Seq[j.GeneticOperator] =
        jConfig.getGeneticOperators.asInstanceOf[java.util.List[j.GeneticOperator]].asScala.view.toSeq

      def size: Int = jConfig.getGeneticOperators.size()
    }

  def build(): Evolver[G] = Evolver.randomGenotype(this, implicitly[Genotype[G]])

}

object EvolverConfig {

  private[helisa] def apply[G: Genotype](fitnessFunction: G => Double): EvolverConfig[G] = new EvolverConfig[G](fitnessFunction)

}

abstract class ConfigurationParameters[Param] private[helisa] () {

  def get(): Seq[Param]

  def add(newValue: Param): Unit

  def size: Int
}

sealed trait ParametersBulkRemove {
  this: ConfigurationParameters[_] =>

  def clear(): Unit
}

sealed trait ParametersSelectiveRemove[Param] extends ParametersBulkRemove {
  this: ConfigurationParameters[Param] =>

  def remove(toRemove: Param): Unit

  def clear(): Unit = get().foreach(remove)
}

private class NaturalSelectorConfigurationParameters(jConfig: j.Configuration, isPre: Boolean)
    extends ConfigurationParameters[j.NaturalSelector]
    with ParametersBulkRemove {
  def get(): Seq[j.NaturalSelector] =
    jConfig.getNaturalSelectors(isPre).iterator().asInstanceOf[java.util.Iterator[j.NaturalSelector]].asScala.toList

  def add(newValue: j.NaturalSelector): Unit = jConfig.addNaturalSelector(newValue, isPre)

  def clear(): Unit = jConfig.removeNaturalSelectors(isPre)

  def size: Int = jConfig.getNaturalSelectorsSize(isPre)
}
