package com.softwaremill.helisa.api

import com.softwaremill.helisa._
import com.softwaremill.helisa.api.Gene.{DoubleGene, IntGene}
import org.scalacheck.Gen
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.OptionValues._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Inside, MustMatchers}

class DecoderSpec extends FlatSpec with MustMatchers with Inside with GeneratorDrivenPropertyChecks {

  it must "decode case classes from a compatible, uniform genotype" in {
    case class SimpleIntGenotype(a: IntGene, b: IntGene)
    case class SimpleIntParams(a: Int, b: Int)

    val g = generatorFor[SimpleIntGenotype](implicit evolver => SimpleIntGenotype(genes.int(0, 5), genes.int(5, 10)))

    forAll(g)(population => {
      population.evolve(1)
      val tested          = population.fittest[SimpleIntParams]
      val fittestGenotype = population.fittestGenotype.value
      inside(tested.value) {
        case SimpleIntParams(a, b) =>
          a must be(fittestGenotype.a.value)
          b must be(fittestGenotype.b.value)
      }
    })

  }

  it must "decode case classes from a compatible, non-uniform genotype" in {
    case class MixedParamsGenotype(a: IntGene, b: DoubleGene)
    case class MixedParams(a: Int, b: Double)

    val g = generatorFor[MixedParamsGenotype](implicit evolver => MixedParamsGenotype(genes.int(0, 5), genes.double(-0.5, 0.5)))

    forAll(g)(population => {
      population.evolve(1)
      val tested          = population.fittest[MixedParams]
      val fittestGenotype = population.fittestGenotype.value
      inside(tested.value) {
        case MixedParams(a, b) =>
          a must be(fittestGenotype.a.value)
          b must be(fittestGenotype.b.value)
      }
    })
  }

  it must "NOT decode case classes from a non-compatible genotype (arity)" in {
    case class SimpleIntGenotype(a: IntGene, b: IntGene)
    case class SimpleIntParams(a: Int)

    val g = generatorFor[SimpleIntGenotype](implicit evolver => SimpleIntGenotype(genes.int(0, 5), genes.int(5, 10)))

    val tested: Population[SimpleIntGenotype] = g.sample.value

    "tested.fittest[SimpleIntParams]" mustNot compile
  }

  it must "NOT decode case classes from a non-compatible genotype (types)" in {
    case class SimpleIntGenotype(a: IntGene, b: IntGene)
    case class SimpleDoubleParams(a: Double, b: Double)

    val g = generatorFor[SimpleIntGenotype](implicit evolver => SimpleIntGenotype(genes.int(0, 5), genes.int(5, 10)))

    val tested: Population[SimpleIntGenotype] = g.sample.value

    "tested.fittest[SimpleIntParams]" mustNot compile
  }

  private def generatorFor[G: Genotype](sample: Evolver[G] => G) = {
    implicit val evolver: Evolver[G] = Evolver[G](_ => 1.0)

    evolver.sampleGenotype = sample(evolver)
    evolver.maxPopulationSize = 1

    Gen.delay(Population.randomGenotype(evolver))
  }
}
