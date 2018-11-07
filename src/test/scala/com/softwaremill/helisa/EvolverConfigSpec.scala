package com.softwaremill.helisa

import org.{jgap => j}
import com.softwaremill.helisa.api.Gene.IntGene
import com.softwaremill.helisa.api.GenotypeValidator
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1}
import org.scalatest.{BeforeAndAfter, FlatSpec, Inside, MustMatchers}
import org.scalatest.OptionValues._

class EvolverConfigSpec extends FlatSpec with BeforeAndAfter with MustMatchers with Inside with TableDrivenPropertyChecks {

  case class BlahGenotype(a: IntGene)

  implicit var tested: EvolverConfig[BlahGenotype] = _

  before {
    tested = EvolverConfig[BlahGenotype](_ => 1.0)
  }

  "The evolver" must "set the fitness function" in {
    tested.jConfig.getFitnessFunction must not be null
  }

  it must "set the sample genotype" in {
    val min    = -1
    val max    = 13
    val sample = BlahGenotype(genes.int(min, max))

    tested.sampleGenotype = sample
    val resultGene = tested.jConfig.getSampleChromosome.getGene(0).asInstanceOf[j.impl.IntegerGene]

    inside(tested.sampleGenotype) {
      case BlahGenotype(a: IntGene) =>
        a.jGene.getLowerBounds must be(min)
        a.jGene.getUpperBounds must be(max)
    }
    resultGene.getLowerBounds must be(min)
    resultGene.getUpperBounds must be(max)
  }

  it must "set maximum population size" in {
    val popSize = 230

    tested.maxPopulationSize = popSize

    tested.maxPopulationSize must be(popSize)
    tested.jConfig.getPopulationSize must be(popSize)
  }

  it must "set minimum population size ratio" in {
    val popSizeRatio = 0.73

    tested.minPopulationSizeRatio = popSizeRatio

    tested.minPopulationSizeRatio must be(popSizeRatio)
    tested.jConfig.getMinimumPopSizePercent must be(popSizeRatio * 100)
  }

  it must "set the random generator" in {
    val gen = new j.impl.GaussianRandomGenerator()

    tested.randomGenerator = gen

    tested.randomGenerator must be(gen)
    tested.jConfig.getRandomGenerator must be(gen)
  }

  it must "set the validator" in {
    val validator = GenotypeValidator[BlahGenotype]((_, _, _) => true)

    tested.validator = validator

    tested.validator.value must be(validator)
  }

  lazy val StandardOperators: TableFor1[j.GeneticOperator] = {
    import geneticOperators._
    Table(
      "operator",
      mutation.default(0.5),
      mutation.gaussian(),
      mutation.inverting(),
      mutation.swapping(0.03),
      mutation.twoWay(0.05),
      crossover.standard(0.3, true),
      crossover.full(0.01),
      crossover.averaging(),
      crossover.greedy()
    )
  }

  lazy val StandardPreSelectors: TableFor1[j.NaturalSelector] = {
    import selectors.pre._
    Table(
      "selector",
      threshold(0.45),
      tournament(4, 0.2),
      weightedRoulette()
    )
  }

  lazy val StandardPostSelectors: TableFor1[j.NaturalSelector] = {
    import selectors.post._
    Table(
      "selector",
      standardPost(),
      best()
    )
  }

  it must "add mutation and crossover operators" in {
    forAll(StandardOperators) { operator =>
      tested.geneticOperators.add(operator)

      tested.geneticOperators.get().lastOption.value must be(operator)
      tested.jConfig.getGeneticOperators.get(tested.jConfig.getGeneticOperators.size() - 1) must be(operator)
    }
  }

  it must "add natural selectors as pre" in {
    forAll(StandardPreSelectors) { selector =>
      tested.naturalSelectorsPreGeneticOperators.add(selector)

      tested.naturalSelectorsPreGeneticOperators.get().lastOption.value must be(selector)
      tested.jConfig.getNaturalSelectors(true).get(tested.jConfig.getNaturalSelectors(true).size() - 1) must be(selector)
    }
  }

  it must "add natural selectors as post" in {
    forAll(StandardPostSelectors) { selector =>
      tested.naturalSelectorsPostGeneticOperators.add(selector)

      tested.naturalSelectorsPostGeneticOperators.get().lastOption.value must be(selector)
      tested.jConfig.getNaturalSelectors(false).get(tested.jConfig.getNaturalSelectors(false).size() - 1) must be(selector)
    }
  }

  it must "remove genetic operators" in {
    val selector = selectors.pre.weightedRoulette()

    tested.naturalSelectorsPostGeneticOperators.add(selector)
    tested.naturalSelectorsPostGeneticOperators.remove(selector)

    tested.naturalSelectorsPostGeneticOperators.get() mustNot contain(selector)
  }

}
