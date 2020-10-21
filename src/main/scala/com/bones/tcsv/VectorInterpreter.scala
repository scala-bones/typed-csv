package com.bones.tcsv

import scala.util.Try


case class RowValues(row: Vector[String]) extends AnyVal

object VectorInterpreter {

    import DataDef._
    import DataList._


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
            line => line.row.getOrError(strDef.index)
    
    def extractInt(intDef: IntDef): RowValues => Either[Errors, Int] =
            line => {
                line.row.getOrError(intDef.index).flatMap(str => {
                    Try {  str.toInt }
                        .toEither.left
                        .map(ex => { error(s"Value ${str} could not be converted to an Int") })
                })                
            }
}



