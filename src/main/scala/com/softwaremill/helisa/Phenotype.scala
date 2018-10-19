package com.softwaremill.helisa

import com.softwaremill.helisa.gene.Gene
import org.{jgap => j}

import scala.annotation.implicitNotFound

// stripMargin cannot be used because annotation processor expects string constants (and ignores the directive otherwise)
// format: off
@implicitNotFound(msg =
"""Phenotype conversion for ${G} -> ${A} not found.

For automatic generation, you need these things:
 The phenotype must be a case class compatible with the provided genotype.
   E.g. if your Genotype G is composed of the following genes

   case class Genotype1(g1: IntGene, g2: StringGene)

   your phenotype A must have fields with their corresponding types:

   case class Phenotype1(value1: Int, value2: String)
""")
// format: on
trait Phenotype[G, A] {
  def convert(g: G): A
}

object Phenotype {

  import shapeless._

  implicit def geneConverterBase[Value, G <: Gene[Value]]: Phenotype[G, Value] = _.value

  implicit val geneConverterHNil: Phenotype[HNil, HNil] = _ => HNil

  implicit def geneConverterHCons[Value, Head, Tail <: HList, VTail <: HList](
      implicit vGC: Phenotype[Head, Value],
      vGT: Phenotype[Tail, VTail]): Phenotype[Head :: Tail, Value :: VTail] = {
    case head :: tail => vGC.convert(head) :: vGT.convert(tail)
  }

  implicit def convertGeneric[G, GRepr <: HList, A, ARepr <: HList](implicit gG: Generic.Aux[G, GRepr],
                                                                    gA: Generic.Aux[A, ARepr],
                                                                    pGtoA: Phenotype[GRepr, ARepr]): Phenotype[G, A] =
    g => gA.from(pGtoA.convert(gG.to(g)))

}
