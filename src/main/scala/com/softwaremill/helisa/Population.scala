package com.softwaremill.helisa

import org.jgap.IChromosome
import org.{jgap => j}
import scala.collection.JavaConverters._

class Population[G: Genotype: Evolver] private (private[helisa] val configuration: Evolver[G]) {

  private[helisa] val jGenotype = j.Genotype.randomInitialGenotype(configuration.jConfig)
  private[helisa] def jPop      = jGenotype.getPopulation

  def evolve(numberOfEvolutions: Int = 1): Population[G] = {
    jGenotype.evolve(numberOfEvolutions)
    this
  }

  def fittest[A: Phenotype[G, ?]]: Option[A] = fittestGenotype.map(_.toPhenotype)

  def fittest[A: Phenotype[G, ?]](num: Int): Seq[A] = fittestGenotypes(num).map(_.toPhenotype)

  def fittestGenotype: Option[G] = Option(jGenotype.getFittestChromosome).flatMap(_.fromJ.toOption)

  def fittestGenotypes(num: Int): Seq[G] =
    jGenotype.getFittestChromosomes(num).asInstanceOf[java.util.List[IChromosome]].asScala.map(_.fromJ).collect {
      case Left(conversionError) => throw new IllegalStateException(conversionError)
      case Right(a)              => a
    }

  def fitnessValue(genotype: G): Double = genotype.toJ.getFitnessValue

  def genotypes: Seq[G] =
    jPop.toChromosomes.toSeq.map(_.fromJ).collect {
      case Left(conversionError) => throw new IllegalStateException(conversionError)
      case Right(a)              => a
    }

  def add(newGenoype: G): Population[G] = {
    jPop.addChromosome(newGenoype.toJ)
    this
  }

  def add(newGenotype: Seq[G]): Population[G] = {
    newGenotype.foreach(add)
    this
  }

  def fill(max: Int): Seq[G] = {
    jGenotype.fillPopulation(max)
    genotypes
  }

  def keepPopSizeConstant(): Unit = jPop.keepPopSizeConstant()

  def applyGeneticOperators(): Population[G] = {
    jGenotype.applyGeneticOperators()
    this
  }

}

object Population {

  def randomGenotype[G: Genotype: Evolver](configuration: Evolver[G]): Population[G] = new Population[G](configuration)

}
