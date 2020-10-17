package com.bones.tcsv

/**
* Used to confirm that all items in a Tuple are of type DataDef[_].
* This is somewhat iquivelant to Tuple.IsMappedBy, however is MappedBy only 
* works if all subtypes are downcast to the base type DataDef[_].  This
* work in the case where we keep the subtypes, eg (StringDef, IntDef) will
* match.
*/
type AllDataDefs[T <: Tuple] = T match {
    case EmptyTuple => DummyImplicit
    case DataDef[_] *: t => AllDataDefs[t]
    case _ => Nothing
}


sealed trait DataDef[A] 
case class StringDef(index: Int) extends DataDef[String]    
case class IntDef(index: Int) extends DataDef[Int]
case class PureDef[A](a: A) extends DataDef[A]


/**
* @tparam T This should be a tuple of DataDef[_]
* @tparam I This represends the InverseMap of the tuple.  For example if we have a tuple
*              (StringDef("Name"), IntDef("Age")) , this is equivelant to (DataDef[String], DataDef[Int])
*              and the InverseMap of this in the context of DataDef is (String,Int)
* @tparam B This is our expected result type after combining the inputs, eg: Person ( case class Person(name: String, age: Int))
* @param f A function responsible for converting the InverseMap of T into the result type, eg: (String,Int) => Person
* @param dataDefs A tuple of DataDefs, eg: (StringDef("Name"), IntDef("Age"))
*/ 
case class MapDef[I,B](
    f: I => B, 
    dataDef: DataDef[I]) extends DataDef[B]
case class ListDef[T<:Tuple](list: DataList[T]) extends DataDef[T]

sealed trait DataList[A<:Tuple] {
    def ::[A](dataDef: DataDef[A]) = Cons(dataDef, this)
    def ++[H<:Tuple](head: DataList[H]) = PlusPlus(this, head)
    def asDef = ListDef(this)
}
case object EmptyList extends DataList[EmptyTuple]
case class Cons[A,T<:Tuple](dataDef: DataDef[A], tail: DataList[T]) extends DataList[A*:T]
case class PlusPlus[H<:Tuple,T<:Tuple](head: DataList[H], tail: DataList[T]) extends DataList[Tuple.Concat[H,T]]



object syntax {

    val dnil = EmptyList

    def constant[A](a: A) = PureDef(() => a)

    def string(index: Int) = 
        StringDef(index)

    def int(index: Int) = 
        IntDef(index)

    /**
    * The type Tuple.InverseMap[T,DataDef] is the tuple of the inner types of DataDef.
    * For instance, if we have a Tuple: (IntDef("int"), StringDef("string")),
    * then InverseMap is (Int,String).
    * 
    * Example useage:
    * val base = (IntDef("Name"), IntDef("Age"))
    * case class Person(name: String, age: Int)
    * combine( (Person.apply _).tupled, base)
    */
    def combine[T<:Tuple,B](f: T => B, t: DataDef[T]): MapDef[T,B] = MapDef(f, t)

}


