package com.bones.tcsv

import scala.util.Try

case class ErrorMessage(message: String) extends AnyVal

/** A non-empty list, _1 is the head, and _2 is the tail. */
type Errors = (ErrorMessage, List[ErrorMessage])
def error(str: String) = (ErrorMessage(str), List.empty)
def concatErrors(e1: Errors, e2: Errors) : Errors = {
    (e1._1, e1._2 ::: e2._1 :: e2._2)
}
def ap2[A,B,C](r1: Either[Errors,A], r2: Either[Errors,B], f: (A,B) => C): Either[Errors,C] = {
    (r1, r2) match {
        case (Left(e1), Left(e2)) => Left(concatErrors(e1,e2))
        case (Left(e), _) => Left(e)
        case (_, Left(e)) => Left(e)
        case (Right(a), Right(b)) => Right(f(a,b))
    }
}


case class RowValues(row: Vector[String]) extends AnyVal

object VectorInterpreter {

    def extract[A](dataDef: DataDef[A]): RowValues => Either[Errors, A] = {
        dataDef match {
            case l: ListDef[Tuple] @unchecked => extractList(l)
            case c: MapDef[t,b] => extractMap[t,b](c)
            case s: StringDef => extractString(s)
            case i: IntDef => extractInt(i)
            case p: PureDef[a] => _ => Right(p.a)
        }
    }

    def extractDefList[T<:Tuple](dataList: DataList[T]): RowValues => Either[Errors,T] = {
        dataList match {
            case EmptyList => row => Right(EmptyTuple.asInstanceOf[T])
            case Cons(a, tail) => {
                val headF = extract(a)
                val tailF = extractDefList(tail)
                row => {
                    val headResult = headF(row) 
                    val tailResult = tailF(row)
                    ap2(headResult, tailResult, _ *: _)
                }
            }
            case PlusPlus(head,tail) => {
                val headF = extractDefList(head)
                val tailF = extractDefList(tail)
                row => {
                    val headResult = headF(row)
                    val tailResult = tailF(row)
                    ap2(headResult, tailResult, _ ++ _)
                }
            }
        }
    }    

    def extractMap[I,B](map: MapDef[I,B]): RowValues => Either[Errors, B] = {
        val fMap = extract(map.dataDef)
        row => fMap(row).map(map.f)
    }

    def extractList[T<:Tuple](listDef: ListDef[T]): RowValues => Either[Errors,T] = {
        extractDefList(listDef.list)
    }

    def extractString(strDef: StringDef): RowValues => Either[Errors, String] =
            line => Right(line.row(strDef.index))
    
    def extractInt(intDef: IntDef): RowValues => Either[Errors, Int] =
            line => {
                val input = line.row(intDef.index)
                Try {  input.toInt }
                    .toEither.left
                    .map(ex => { error(s"Value ${input} could not be converted to an Int") })
            }
}



