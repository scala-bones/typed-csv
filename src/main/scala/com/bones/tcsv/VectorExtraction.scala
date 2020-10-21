package com.bones.tcsv

import scala.util.Try
import DataDef._
import DataList._

trait VectorExctraction[T] {
    def extract(row: Vector[String]): Either[Errors,T]
}

case class StringExtraction(stringDef: StringDef) extends VectorExctraction[String] {
    def extract(row: Vector[String]): Either[Errors, String] = 
        row.getOrError(stringDef.index)
}

case class IntExtraction(intDef: IntDef) extends VectorExctraction[Int] {
    def extract(row: Vector[String]): Either[Errors, Int] = 
        row.getOrError(intDef.index).flatMap(str => {
                    Try {  str.toInt }
                        .toEither.left
                        .map(ex => { error(s"Value ${str} could not be converted to an Int") })
                })                
}

case class MapExtraction[I,B](mapDef: MapDef[I,B], iExtractor: VectorExctraction[I]) extends VectorExctraction[B] {
    def extract(row:Vector[String]): Either[Errors, B] =  {
        val i = iExtractor.extract(row)
        i.map(mapDef.f)
    }
}

case class PureExtraction[A](pure: PureDef[P]) extends VectorExctraction[A] {
    def extract(row: Vector[String]): Either[Errors, A] = Right(pure.a)
}

case class ListExtraction[T<:Tuple](list: ListDef[T]) extends VectorExctraction[T] {

    def extractDl(dl: DataList[T]): Either[Errors, T] = {
        dl match {
            case Nil => Right(EmptyTuple.asInstanceOf[T])
            case Cons(dataDef, tail) => ???
        }
    }

    def extract(row: Vector[String]): Either[Errors, T] = {

    }
}