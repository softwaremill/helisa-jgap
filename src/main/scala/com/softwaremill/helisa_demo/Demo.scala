package com.softwaremill.helisa_demo

import com.softwaremill.helisa._
import com.softwaremill.helisa.api.Gene.{IntGene, IntOfMultipleGene}
import com.softwaremill.helisa.api.{Gene, GenotypeValidator, Population}

object Demo extends App {

  val fitnessFunction = (cannyParams: CannyGenes) =>
    ((cannyParams.low.value + cannyParams.high.value + cannyParams.blur.value) % 8.0) + 1.0

  implicit val evolverConfig: EvolverConfig[CannyGenes] = EvolverConfig(fitnessFunction)

  evolverConfig.validator = GenotypeValidator((gene: Gene[_], genotype: CannyGenes, index: Int) => {
    val value = gene.value.asInstanceOf[Int]
    if (index == 2) {
      true
    } else {
      val (low, high) = if (index == 0) (value, genotype.high.value) else (genotype.low.value, value)
      high > low
    }
  })

  evolverConfig.sampleGenotype = CannyGenes(genes.int(0, 255), genes.int(0, 255), genes.intOfMultiple(0, 12, 2))
  evolverConfig.maxPopulationSize = 100

  val evolver = evolverConfig.build()

  val pop = evolver.streamScalaStdLib().take(1000).head

  println(pop.fittest[CannyParameters])

}

case class CannyGenes(low: IntGene, high: IntGene, blur: IntOfMultipleGene)

case class CannyParameters(low: Int, high: Int, blur: Int)
