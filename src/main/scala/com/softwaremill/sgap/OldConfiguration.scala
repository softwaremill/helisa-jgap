package com.softwaremill.sgap

import com.softwaremill.sgap.gene.Gene
import org.{jgap => j}
import shapeless._
import shapeless.ops.hlist._
class OldConfiguration[Chromosome, Repr <: HList, ZipOut <: HList, ZipConstOut <: HList](sampleChromosome: Chromosome,
                                                                                         fitnessFunction: (Chromosome) => Double)(
    implicit g: Generic.Aux[Chromosome, Repr],
    toTraversableAux: ToTraversable.Aux[Repr, List, Gene[_, _ <: j.Gene]],
    zipWithIndex: ZipWithIndex.Aux[Repr, ZipOut],
    zipConst: ZipConst.Aux[org.jgap.IChromosome, ZipOut, ZipConstOut],
    reverseMapper: Mapper.Aux[constructChromosome.type, ZipConstOut, Repr]) {

  private object jInternal {
    private val config = new j.Configuration

    private val chromosome = {
      val genes = g.to(sampleChromosome).toList[Gene[_, _ <: j.Gene]].map(_.jGene)
      new j.Chromosome(config, genes.toArray)
    }

    private val fitness: j.FitnessFunction =
      (iChromosome: j.IChromosome) => {

        val values = g.to(sampleChromosome).zipWithIndex.zipConst(iChromosome).map(constructChromosome)

        val result: Chromosome = g.from(values)

        fitnessFunction(result)
      }

    config.setSampleChromosome(chromosome)

    config.setFitnessFunction(fitness)
  }

}

object constructChromosome extends Poly1 {
  implicit def caseGene[JGene <: j.Gene, SGene <: Gene[_, JGene] { val copy: JGene => SGene }]
    : Case.Aux[((SGene, Int), j.IChromosome), SGene] = at {
    case ((sGene, index), chromosome) => sGene.copy(chromosome.getGene(index).asInstanceOf[JGene])
  }
}
