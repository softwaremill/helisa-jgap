package com.softwaremill.sgapdemo

import com.softwaremill.sgap._
import com.softwaremill.sgap.gene.{Gene, IntGene, MultipleIntGene}
import org.jgap.impl.{IntegerGene, MutipleIntegerGene}
import org.jgap.IChromosome

object Demo extends App {

  //works, but not supported
  //val chromosomeCanny = implicitly[Chromosome[CannyParameters]]
  val baseChromosomeCanny: Chromosome[CannyParameters] = caseClassChromosome

  //temporary until we get the CC conversion working
  implicit val chromosomeCanny: Chromosome[CannyParameters] = new Chromosome[CannyParameters] {
    def fromJ(jChromo: IChromosome): CannyParameters =
      CannyParameters(
        IntGene(jChromo.getGene(0).asInstanceOf[IntegerGene]),
        IntGene(jChromo.getGene(1).asInstanceOf[IntegerGene]),
        MultipleIntGene(jChromo.getGene(2).asInstanceOf[MutipleIntegerGene])
      )

    def genes(a: CannyParameters): Vector[Gene[_, _ <: org.jgap.Gene]] = baseChromosomeCanny.genes(a)
  }

  val fitnessFunction = (cannyParams: CannyParameters) =>
    if (cannyParams.low.value > cannyParams.high.value) 0.0
    else
      ((cannyParams.low.value + cannyParams.high.value + cannyParams.blur.value) % 8.0) + 1.0

  implicit val config: Configuration[CannyParameters] = Configuration(fitnessFunction)

  config.sampleChromosome = CannyParameters(IntGene(0, 255), IntGene(0, 255), MultipleIntGene(0, 12, 2))
  config.populationSize = 100

  val genotype = Genotype.randomGenotype(config)

  genotype.evolve(1000)

  println(genotype.fittestChromosome)

}

case class CannyParameters(low: IntGene, high: IntGene, blur: MultipleIntGene)
