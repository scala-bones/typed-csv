package com.bones.tcsv

/**
* Responsible for returning a Description of the DataDef and what contents are
* expected.
*/
object DescribeInterpreter {
    import DataDef._
    import DataList._
    case class Index(index: Int) extends AnyVal
    case class Description(description: String) extends AnyVal

    def describe[A](ddef: DataDef[A]): List[Description] = {
        val desc: List[(Index,Description)] = toIndexDesc(ddef)
        desc.sortBy(_._1.index).map(_._2)
    }

    def describeList[T<:Tuple](ddef: DataList[T]): List[Description] = {
        val desc: List[(Index,Description)] = dataList(ddef)
        desc.sortBy(_._1.index).map(_._2)
    }

    def toIndexDesc[A](ddef: DataDef[A]): List[(Index,Description)] = {
        ddef match {
            case l: ListDef[Tuple] @unchecked => listDesc(l)
            case c: MapDef[t,b] => mappedDesc[t,b](c)
            case s: StringDef => strIndexDesc(s)
            case i: IntDef => intIndexDesc(i)
            case p: PureDef[a] => List.empty
        }
    }

    def dataList[T<:Tuple](dlist: DataList[T]): List[(Index, Description)] =
        dlist match {
            case EmptyList => List.empty
            case Cons(head, tail) => toIndexDesc(head) ::: dataList(tail)
            case PlusPlus(head,tail) => dataList(head) ::: dataList(tail)
        }

    def listDesc[T<:Tuple](ldef: ListDef[T]): List[(Index,Description)] = 
        dataList(ldef.list)

    def mappedDesc[T,B](mdef: MapDef[T,B]): List[(Index, Description)] = 
        toIndexDesc(mdef.dataDef)

    def strIndexDesc(sdef: StringDef): List[(Index, Description)] =
        List( ( Index(sdef.index), Description(s"Expect index ${sdef.index} to be of type String")))

    def intIndexDesc(sdef: IntDef): List[(Index, Description)] =
        List( ( Index(sdef.index), Description(s"Expect index ${sdef.index} to be of type Int")))


}