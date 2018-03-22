package com.softwaremill.sgapdemo

import com.softwaremill.sgap._
import com.softwaremill.sgap.gene.{IntGene, IntOfMultipleGene}

object Demo extends App {

  val fitnessFunction = (cannyParams: CannyParameters) =>
    if (cannyParams.low.value > cannyParams.high.value) 0.0
    else
      ((cannyParams.low.value + cannyParams.high.value + cannyParams.blur.value) % 8.0) + 1.0

  implicit val config: Configuration[CannyParameters] = Configuration(fitnessFunction)

  config.sampleChromosome = CannyParameters(genes.int(0, 255), genes.int(0, 255), genes.intOfMultiple(0, 12, 2))
  config.maxPopulationSize = 100

  val genotype = Genotype.randomGenotype(config)

  genotype.evolve(1000)

  println(genotype.fittestChromosome)

}

case class CannyParameters(low: IntGene, high: IntGene, blur: IntOfMultipleGene)
