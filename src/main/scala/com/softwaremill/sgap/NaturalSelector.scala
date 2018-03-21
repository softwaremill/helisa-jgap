package com.softwaremill.sgap

import alleycats.Pure
import cats.MonoidK
import cats.instances.all._
import alleycats.std.all._
import cats.syntax.semigroupk._
import org.jgap.IChromosome
import org.{jgap => j}

import scala.collection.JavaConverters._
import scala.collection.immutable.Set

object NaturalSelector {


  def apply[A: Chromosome: Configuration](select: (Seq[A], Int) => Seq[A], doublettesAllowed: Boolean) =
    if (doublettesAllowed)
      new NaturalSelector[A, List](select, doublettesAllowed)
    else
      new NaturalSelector[A, Set](select, doublettesAllowed)

}

class NaturalSelector[A: Chromosome: Configuration, Col[_] <: Iterable[j.IChromosome]: MonoidK : Pure] private (
    doSelect: (Seq[A], Int) => Seq[A],
    doublettesAllowed: Boolean)
    extends j.NaturalSelector {

  protected var jChromos: Col[j.IChromosome] = MonoidK[Col].empty

  def returnsUniqueChromosomes(): Boolean = !doublettesAllowed

  def select(a_howManyToSelect: Int, a_from_population: j.Population, a_to_population: j.Population): Unit = {
    a_from_population.getChromosomes.asScala.foreach(add)

    //TODO: solve the "it cannot happen here problem with the conversions
    val input  = jChromos.map(_.fromJ).collect { case Right(a) => a }.toSeq
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

