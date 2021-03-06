package com.softwaremill.helisa.api

import com.softwaremill.helisa._
import org.jgap.IChromosome
import org.{jgap => j}

import scala.collection.JavaConverters._

class Population[G: Genotype: EvolverConfig] private[helisa] (private[helisa] val jGenotype: j.Genotype) {

  private[helisa] def jPop = jGenotype.getPopulation

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

  def add(newGenotype: G): Population[G] = {
    jPop.addChromosome(newGenotype.toJ)
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
