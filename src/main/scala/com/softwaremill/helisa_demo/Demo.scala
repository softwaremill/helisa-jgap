package com.softwaremill.helisa_demo

import com.softwaremill.helisa._
import com.softwaremill.helisa.api.Gene.{IntGene, IntOfMultipleGene}
import com.softwaremill.helisa.api.{Gene, GenotypeValidator}

object Demo extends App {

  val fitnessFunction = (cannyParams: CannyGenes) =>
    ((cannyParams.low.value + cannyParams.high.value + cannyParams.blur.value) % 8.0) + 1.0

  val validator = GenotypeValidator((gene: Gene[_], genotype: CannyGenes, index: Int) => {
    val value = gene.value.asInstanceOf[Int]
    if (index == 2) {
      true
    } else {
      val (low, high) = if (index == 0) (value, genotype.high.value) else (genotype.low.value, value)
      high > low
    }
  })

  val evolver = Evolver[CannyGenes](fitnessFunction,
                                    implicit c => CannyGenes(genes.int(0, 255), genes.int(0, 255), genes.intOfMultiple(0, 12, 2)),
                                    100,
                                    validator = Some(validator))

  val pop = evolver.streamScalaStdLib().take(1000).head

  println(pop.fittest[CannyParameters])

}

case class CannyGenes(low: IntGene, high: IntGene, blur: IntOfMultipleGene)

case class CannyParameters(low: Int, high: Int, blur: Int)
