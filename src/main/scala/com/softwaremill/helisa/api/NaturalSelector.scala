package com.softwaremill.helisa.api

import alleycats.Pure
import alleycats.std.all._
import cats.instances.all._
import cats.syntax.foldable._
import cats.syntax.semigroupk._
import cats.{MonoidK, Traverse}
import com.softwaremill.helisa._
import org.jgap.IChromosome
import org.{jgap => j}

import scala.collection.JavaConverters._
import scala.collection.immutable.Set
import scala.language.higherKinds

object NaturalSelector {

  def apply[G: Genotype: EvolverConfig](select: (Seq[G], Int) => Seq[G], doublettesAllowed: Boolean) =
    if (doublettesAllowed)
      new NaturalSelector[G, List](select, doublettesAllowed)
    else
      new NaturalSelector[G, Set](select, doublettesAllowed)

  object selectors {

    object pre {
      def threshold(rate: Double)(implicit c: EvolverConfig[_]) = new j.impl.ThresholdSelector(c.jConfig, rate)

      def tournament(tournamentSize: Int, bestSelectionProbability: Double)(implicit c: EvolverConfig[_]) =
        new j.impl.TournamentSelector(c.jConfig, tournamentSize, bestSelectionProbability)

      def weightedRoulette()(implicit c: EvolverConfig[_]) = new j.impl.WeightedRouletteSelector(c.jConfig)
    }

    object post {
      def standardPost()(implicit c: EvolverConfig[_]) = new j.impl.StandardPostSelector(c.jConfig)

      def best(originalRate: Double = 0.90)(implicit c: EvolverConfig[_]) =
        new j.impl.BestChromosomesSelector(c.jConfig, originalRate)
    }

  }

}

class NaturalSelector[G: Genotype: EvolverConfig, Col[_]: MonoidK: Pure: Traverse] private (doSelect: (Seq[G], Int) => Seq[G],
                                                                                            doublettesAllowed: Boolean)
    extends j.NaturalSelector {

  private var jChromos: Col[j.IChromosome] = MonoidK[Col].empty

  def returnsUniqueChromosomes(): Boolean = !doublettesAllowed

  def select(a_howManyToSelect: Int, a_from_population: j.Population, a_to_population: j.Population): Unit = {
    a_from_population.getChromosomes.asScala.foreach(add)

    //TODO: solve the "it cannot happen here problem with the conversions
    val input  = jChromos.toList.map(_.fromJ).collect { case Right(a) => a }.toSeq
    val output = doSelect(input, a_howManyToSelect)
    for (outJChromo ← output.map(_.toJ)) {
      outJChromo.setIsSelectedForNextGeneration(true)
      a_to_population.addChromosome(outJChromo)
    }
  }

  def empty(): Unit = jChromos = MonoidK[Col].empty

  def add(a_chromosomeToAdd: IChromosome): Unit = {
    a_chromosomeToAdd.setIsSelectedForNextGeneration(false)
    jChromos = jChromos.combineK(Pure[Col].pure(a_chromosomeToAdd))
  }
}
