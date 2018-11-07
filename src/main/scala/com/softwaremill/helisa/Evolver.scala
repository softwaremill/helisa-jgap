package com.softwaremill.helisa

import akka.NotUsed
import com.softwaremill.helisa.api.{Genotype, Population}
import akka.stream.scaladsl.Source
import cats.effect.Async
import cats.syntax.option._
import fs2.{Stream => Fs2Stream}
import org.{jgap => j}

import scala.language.higherKinds

class Evolver[G: Genotype: EvolverConfig] private[helisa] (private val jGenotype: j.Genotype) {

  private[helisa] def evolve(numberOfEvolutions: Int = 1): Evolver[G] = {
    jGenotype.evolve(numberOfEvolutions)
    this
  }

  private[helisa] def population: Population[G] = new Population[G](jGenotype)

  def streamScalaStdLib(): Stream[Population[G]] = Stream.iterate(this)(_.evolve(1)).map(_.population)

  def source(): Source[Population[G], NotUsed] =
    Source.unfold(this)(eH => (eH.evolve(1) -> eH.population).some)

  val akkaStreamSource: () => Source[Population[G], NotUsed] = source _

  def fs2[F[_]: Async](): Fs2Stream[F, Population[G]] =
    Fs2Stream.iterate(this)(_.evolve(1)).map(_.population)

}

object Evolver {

  private[helisa] def randomGenotype[G](implicit config: EvolverConfig[G], _genotype: Genotype[G]): Evolver[G] =
    new Evolver[G](j.Genotype.randomInitialGenotype(config.jConfig))

}
