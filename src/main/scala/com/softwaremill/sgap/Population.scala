package com.softwaremill.sgap

class Population[A : Chromosome] private (genotype: Genotype[A]) {

  private def config = genotype.configuration
  private def jPop = genotype.jGenotype.getPopulation

  def += (newChromosome: A): Population[A] = {
    jPop.addChromosome(newChromosome.toJ(config))
    this
  }

  def ++=(other: Population[A]): Population[A] = {
    jPop.addChromosomes(other.jPop)
    this
  }

}

object Population {

//  def apply[A: Chromosome]

}
