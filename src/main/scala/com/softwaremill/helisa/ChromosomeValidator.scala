package com.softwaremill.helisa
import com.softwaremill.helisa.gene.Gene
import org.{jgap => j}


abstract class ChromosomeValidator[A: Chromosome : EvolutionRun] {

  def validate(gene: Gene[_, _], chromosome: A, index: Int): Boolean

  def toJ: j.IGeneConstraintChecker = {(gene: j.Gene, value: Any, chromosome: j.IChromosome, index: Int) => {
    if(value == null) true //workaround for j.Chromosome:1879
    else
    chromosome.fromJ.map(chromo => validate(Gene.fromJ(gene), chromo, index)).getOrElse(false)
  }}

}


object ChromosomeValidator {

  //for some reason this is necessary (default conversion doesn't work - maybe because this is an AC not a trait?
  def apply[A : Chromosome : EvolutionRun](v: (Gene[_, _], A, Int) =>  Boolean): ChromosomeValidator[A] = new ChromosomeValidator[A] {
    override def validate(gene: Gene[_, _], chromosome: A, index: Int): Boolean = v(gene, chromosome, index)
  }
}