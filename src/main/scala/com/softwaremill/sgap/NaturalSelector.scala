package com.softwaremill.sgap

import org.jgap.IChromosome
import org.{jgap => j}
import cats.Monoid
import cats.instances.all._

import scala.collection.JavaConverters._

object NaturalSelector {

  def apply[A: Chromosome: Configuration](select: (Seq[A], Int) => Seq[A], doublettesAllowed: Boolean): NaturalSelector[A, _] =
    if (doublettesAllowed)
      new NaturalSelector[A, List[j.IChromosome]](select, doublettesAllowed, new ListChromosomeAccumulator)
    else
      new NaturalSelector[A, Set[j.IChromosome]](select, doublettesAllowed, new SetChromosomeAccumulator)

}

class NaturalSelector[A: Chromosome: Configuration, Col <: Iterable[j.IChromosome]: Monoid] private (
    doSelect: (Seq[A], Int) => Seq[A],
    doublettesAllowed: Boolean,
    accumulator: ChromosomeAccumulator[Col])
    extends j.NaturalSelector {

  protected var jChromos: Col = Monoid[Col].empty

  def returnsUniqueChromosomes(): Boolean = !doublettesAllowed

  def select(a_howManyToSelect: Int, a_from_population: j.Population, a_to_population: j.Population): Unit = {
    a_from_population.getChromosomes.asScala.foreach(add)

    //TODO: solve the "it cannot happen here problem with the conversions
    val input  = jChromos.map(_.fromJ).collect { case Right(a) => a }.toSeq
    val output = doSelect(input, a_howManyToSelect)
    for (outJChromo ← output.map(_.toJ(implicitly[Configuration[A]]))) { //TODO: make implicit in caller
      outJChromo.setIsSelectedForNextGeneration(true)
      a_to_population.addChromosome(outJChromo)
    }
  }

  def empty(): Unit = jChromos = Monoid[Col].empty

  def add(a_chromosomeToAdd: IChromosome): Unit =
    jChromos = accumulator(jChromos)(a_chromosomeToAdd)
}

private trait ChromosomeAccumulator[Col <: Iterable[j.IChromosome]] {

  def apply(current: Col)(chromo: j.IChromosome): Col

}

private class ListChromosomeAccumulator extends ChromosomeAccumulator[List[j.IChromosome]] {
  def apply(current: List[j.IChromosome])(chromo: j.IChromosome): List[j.IChromosome] = current :+ chromo
}

private class SetChromosomeAccumulator extends ChromosomeAccumulator[Set[j.IChromosome]] {
  def apply(current: Set[j.IChromosome])(chromo: j.IChromosome): Set[j.IChromosome] = current + chromo
}
