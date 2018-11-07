package com.softwaremill.helisa

import com.softwaremill.helisa.api.Gene.IntGene
import com.softwaremill.helisa.api.Population
import org.reactivestreams.{Subscriber, Subscription}
import org.scalatest.OptionValues._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Inside, MustMatchers}

import scala.concurrent.{Await, Promise}
import scala.concurrent.duration._

class EvolutionApiSpec extends FlatSpec with MustMatchers with Inside with GeneratorDrivenPropertyChecks {
  case class TestGenotype(a: IntGene)
  case class TestPhenotype(a: Int)

  val TargetValue = 42

  def tested: Evolver[TestGenotype] = {

    val fitnessFunction = (t: TestGenotype) => 100 / (TargetValue.toDouble - t.a.value).abs

    Evolver[TestGenotype](fitnessFunction, implicit c => TestGenotype(genes.int(0, 100)), 1000)
  }

  "The Evolver" must "produce a valid standard Scala stream" in {
    val finalPop = tested.streamScalaStdLib().drop(100).head

    finalPop.fittestValue must be(TargetValue)
  }

  it must "produce a valid publisher" in {
    import cats.syntax.option._

    val finalPop = Promise[Population[TestGenotype]]()

    tested
      .publisher()
      .subscribe(new Subscriber[Population[TestGenotype]] {

        var sub: Option[Subscription] = none
        var counter                   = 1

        override def onSubscribe(s: Subscription): Unit = {
          sub = s.some
          s.request(100)
        }
        override def onNext(t: Population[TestGenotype]): Unit =
          if (counter < 100) {
            counter += 1
          } else {
            sub.foreach(_.cancel())
            finalPop.success(t)
          }
        override def onError(t: Throwable): Unit = ()
        override def onComplete(): Unit          = ()
      })

    Await.result(finalPop.future, 20 seconds).fittestValue must be(TargetValue)
  }

  it must "produce a valid Akka Stream source" in {
    import akka.actor.ActorSystem
    import akka.stream.ActorMaterializer
    import akka.stream.scaladsl.Sink

    implicit val actorSystem = ActorSystem()
    implicit val am          = ActorMaterializer()

    val finalPop = Await.result(tested.source().drop(100).runWith(Sink.head), 20 seconds)

    finalPop.fittestValue must be(TargetValue)
  }

  it must "produce a valid fs2 stream" in {
    import cats.effect._

    val finalPop = tested.fs2[IO]().drop(100).head.compile.last.unsafeRunSync().value

    finalPop.fittestValue must be(TargetValue)
  }

  implicit class TestPopOps(pop: Population[TestGenotype]) {

    def fittestValue = pop.fittest[TestPhenotype].value.a
  }
}
