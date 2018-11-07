package com.softwaremill.helisa

import akka.NotUsed
import com.softwaremill.helisa.api.{Genotype, GenotypeValidator, Population}
import akka.stream.scaladsl.{Keep, Sink, Source}
import cats.effect.Async
import cats.syntax.option._
import fs2.{Stream => Fs2Stream}
import org.jgap.RandomGenerator
import org.jgap.impl.StockRandomGenerator
import org.reactivestreams.Publisher
import org.{jgap => j}

import scala.language.higherKinds

class Evolver[G: Genotype: EvolverConfig] private[helisa] (private val jGenotype: j.Genotype) {

  private[helisa] def evolve(numberOfEvolutions: Int = 1): Evolver[G] = {
    jGenotype.evolve(numberOfEvolutions)
    this
  }

  private[helisa] def population: Population[G] = new Population[G](jGenotype)

  def iterator(): Iterator[Population[G]] = Iterator.iterate(this)(_.evolve(1)).map(_.population)

  def streamScalaStdLib(): Stream[Population[G]] = iterator().toStream

  def source(): Source[Population[G], NotUsed] =
    Source.unfold(this)(eH => (eH.evolve(1) -> eH.population).some)

  val akkaStreamSource: () => Source[Population[G], NotUsed] = source

  def fs2[F[_]: Async](): Fs2Stream[F, Population[G]] =
    Fs2Stream.iterate(this)(_.evolve(1)).map(_.population)

  def publisher(): Publisher[Population[G]] = {
    import akka.actor.ActorSystem
    import akka.stream.ActorMaterializer
    import scala.concurrent.ExecutionContext

    implicit val as: ActorSystem       = ActorSystem(s"helisa_${System.currentTimeMillis()}")
    implicit val am: ActorMaterializer = ActorMaterializer()
    implicit val ec: ExecutionContext  = as.dispatcher

    val (watch, publisher) = source().watchTermination()(Keep.right).toMat(Sink.asPublisher(true))(Keep.both).run()

    watch.onComplete { _ =>
      am.shutdown()
      as.terminate()
    }

    publisher
  }

}

object Evolver {

  def apply[G: Genotype](fitnessFunction: G => Double,
                         sampleGenotype: EvolverConfig[G] => G,
                         maxPopulationSize: Int,
                         validator: Option[GenotypeValidator[G]] = None,
                         minPopulationSizeRatio: Double = 0.0,
                         randomGenerator: RandomGenerator = new StockRandomGenerator,
                         selectorsPre: EvolverConfig[G] => List[j.NaturalSelector] = (_: EvolverConfig[G]) => List.empty,
                         selectorsPost: EvolverConfig[G] => List[j.NaturalSelector] = (c: EvolverConfig[G]) =>
                           List(selectors.post.best()(c)),
                         operators: EvolverConfig[G] => List[j.GeneticOperator] = (c: EvolverConfig[G]) =>
                           List(geneticOperators.crossover.standard()(c), geneticOperators.mutation.default()(c))): Evolver[G] = {
    implicit val config: EvolverConfig[G] = EvolverConfig(fitnessFunction)

    config.sampleGenotype = sampleGenotype(config)
    config.maxPopulationSize = maxPopulationSize
    validator.foreach(config.validator = _)

    config.minPopulationSizeRatio = minPopulationSizeRatio
    config.randomGenerator = randomGenerator

    config.naturalSelectorsPre.clear()
    selectorsPre(config).foreach(config.naturalSelectorsPre.add)

    config.naturalSelectorsPost.clear()
    selectorsPost(config).foreach(config.naturalSelectorsPost.add)

    config.geneticOperators.clear()
    operators(config).foreach(config.geneticOperators.add)

    config.build()
  }

  private[helisa] def randomGenotype[G](implicit config: EvolverConfig[G], _genotype: Genotype[G]): Evolver[G] =
    new Evolver[G](j.Genotype.randomInitialGenotype(config.jConfig))

}
