package com.softwaremill.helisa_demo

import com.softwaremill.helisa._
import com.softwaremill.helisa.gene.Gene.{IntGene, IntOfMultipleGene}
import com.softwaremill.helisa.gene.Gene

object Demo extends App {

  val fitnessFunction = (cannyParams: CannyParameters) =>
    ((cannyParams.low.value + cannyParams.high.value + cannyParams.blur.value) % 8.0) + 1.0

  implicit val evolver: Evololver[CannyParameters] = Evololver(fitnessFunction)


  evolver.validator = ChromosomeValidator((gene: Gene[_], chromosome: CannyParameters, index: Int) => {
    val value = gene.value.asInstanceOf[Int]
    if (index == 2) {
      true
    } else {
      val (low, high) = if (index == 0) (value, chromosome.high.value) else (chromosome.low.value, value)
      high > low
    }
  })


  evolver.sampleChromosome = CannyParameters(genes.int(0, 255), genes.int(0, 255), genes.intOfMultiple(0, 12, 2))
  evolver.maxPopulationSize = 100

  val genotype = Population.randomGenotype(evolver)

  genotype.evolve(1000)

  println(genotype.fittestChromosome)

}

case class CannyParameters(low: IntGene, high: IntGene, blur: IntOfMultipleGene)
