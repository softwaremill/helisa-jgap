package com.softwaremill.sgap

import org.jgap.IChromosome
import org.{jgap => j}
import scala.collection.JavaConverters._

class Genotype[A: Chromosome] private (private[sgap] val configuration: Configuration[A]) {

  private[sgap] val jGenotype = j.Genotype.randomInitialGenotype(configuration.jConfig)
  private[sgap] def jPop      = jGenotype.getPopulation

  def evolve(numberOfEvolutions: Int = 1): Genotype[A] = {
    jGenotype.evolve(numberOfEvolutions)
    this
  }

  def fittestChromosome: Option[A] = Option(jGenotype.getFittestChromosome).flatMap(_.fromJ.toOption)

  def fittestChromosomes(num: Int): Seq[A] =
    jGenotype.getFittestChromosomes(num).asInstanceOf[java.util.List[IChromosome]].asScala.map(_.fromJ).collect {
      case Left(conversionError) => throw new IllegalStateException(conversionError)
      case Right(a)              => a
    }

  def fitnessValue(chromosome: A): Double = chromosome.toJ(configuration).getFitnessValue

  def population: Seq[A] =
    jPop.toChromosomes.toSeq.map(_.fromJ).collect {
      case Left(conversionError) => throw new IllegalStateException(conversionError)
      case Right(a)              => a
    }

  def add(newChromosome: A): Genotype[A] = {
    jPop.addChromosome(newChromosome.toJ(configuration))
    this
  }

  def add(newChromosomes: Seq[A]): Genotype[A] = {
    newChromosomes.foreach(add)
    this
  }

  def fillPopulation(max: Int): Seq[A] = {
    jGenotype.fillPopulation(max)
    population
  }

  def keepPopSizeConstant(): Unit = jPop.keepPopSizeConstant()

  def applyGeneticOperators(): Genotype[A] = {
    jGenotype.applyGeneticOperators()
    this
  }

}

object Genotype {

  def randomGenotype[A: Chromosome](configuration: Configuration[A]): Genotype[A] = new Genotype[A](configuration)

}
