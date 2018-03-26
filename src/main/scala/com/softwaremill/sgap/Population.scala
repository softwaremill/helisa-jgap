package com.softwaremill.sgap

import org.jgap.IChromosome
import org.{jgap => j}
import scala.collection.JavaConverters._

class Population[A: Chromosome : EvolutionRun] private(private[sgap] val configuration: EvolutionRun[A]) {

  private[sgap] val jGenotype = j.Genotype.randomInitialGenotype(configuration.jConfig)
  private[sgap] def jPop      = jGenotype.getPopulation

  def evolve(numberOfEvolutions: Int = 1): Population[A] = {
    jGenotype.evolve(numberOfEvolutions)
    this
  }

  def fittestChromosome: Option[A] = Option(jGenotype.getFittestChromosome).flatMap(_.fromJ.toOption)

  def fittestChromosomes(num: Int): Seq[A] =
    jGenotype.getFittestChromosomes(num).asInstanceOf[java.util.List[IChromosome]].asScala.map(_.fromJ).collect {
      case Left(conversionError) => throw new IllegalStateException(conversionError)
      case Right(a)              => a
    }

  def fitnessValue(chromosome: A): Double = chromosome.toJ.getFitnessValue

  def genotypes: Seq[A] =
    jPop.toChromosomes.toSeq.map(_.fromJ).collect {
      case Left(conversionError) => throw new IllegalStateException(conversionError)
      case Right(a)              => a
    }

  def add(newChromosome: A): Population[A] = {
    jPop.addChromosome(newChromosome.toJ)
    this
  }

  def add(newChromosomes: Seq[A]): Population[A] = {
    newChromosomes.foreach(add)
    this
  }

  def fill(max: Int): Seq[A] = {
    jGenotype.fillPopulation(max)
    genotypes
  }

  def keepPopSizeConstant(): Unit = jPop.keepPopSizeConstant()

  def applyGeneticOperators(): Population[A] = {
    jGenotype.applyGeneticOperators()
    this
  }

}

object Population {

  def randomGenotype[A: Chromosome : EvolutionRun](configuration: EvolutionRun[A]): Population[A] = new Population[A](configuration)

}
