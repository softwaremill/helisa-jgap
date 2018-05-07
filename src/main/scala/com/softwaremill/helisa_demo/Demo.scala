package com.softwaremill.helisa_demo

import com.softwaremill.helisa._
import com.softwaremill.helisa.gene.{Gene, IntGene, IntOfMultipleGene}

object Demo extends App {

  val fitnessFunction = (cannyParams: CannyParameters) =>
    ((cannyParams.low.value + cannyParams.high.value + cannyParams.blur.value) % 8.0) + 1.0

  implicit val config: EvolutionRun[CannyParameters] = EvolutionRun(fitnessFunction)


  //^necessary because otherwise
  config.validator = ChromosomeValidator((gene: Gene[_, _], chromosome: CannyParameters, index: Int) => {
    val value = gene.value.asInstanceOf[Int]
    if (index == 2) {
      true
    } else {
      val (low, high) = if (index == 0) (value, chromosome.high.value) else (chromosome.low.value, value)
      high > low
    }
  })


  config.sampleChromosome = CannyParameters(genes.int(0, 255), genes.int(0, 255), genes.intOfMultiple(0, 12, 2))
  config.maxPopulationSize = 100

  val genotype = Population.randomGenotype(config)

  genotype.evolve(1000)

  println(genotype.fittestChromosome)

}

case class CannyParameters(low: IntGene, high: IntGene, blur: IntOfMultipleGene)
