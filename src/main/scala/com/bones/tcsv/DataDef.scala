package com.bones.tcsv

case class ErrorMessage(message: String) extends AnyVal

// Can not use GADTs.  they are considered to have multiple constructors, but one return type.
//  Because of this design, we lose the recursive data structure in our Combine[] class.
// enum DataDef[A] {

//     def map[B](f: A => B): DataDef[B] = Map(f, this)

//     case StringDef(header: String) extends DataDef[String]    
//     case IntDef(headerName: String) extends DataDef[Int]
//     case BigDecimalDef(headerName: String) extends DataDef[BigDecimal]
//     case Pure[A](a: () => A) extends DataDef[A]
//     case Map[A,B](f: A => B, dataDef: DataDef[A]) extends DataDef[B]
//     case Combine[T<:NonEmptyTuple,I<:Tuple.InverseMap[T,DataDef],B](f: I => B, dataDefs: T)(using Tuple.IsMappedBy[DataDef][T]) extends DataDef[B]
//     // case Product[A,B](da: DataDef[A], db: DataDef[B]) extends DataDef[(A,B)]  // Semigroup
// }

type AllDataDefs[T <: Tuple] = T match {
    case EmptyTuple => DummyImplicit
    case DataDef[_] *: t => AllDataDefs[t]
    case _ => Nothing
}

sealed trait DataDef[A] {
    def map[B](f: A => B): DataDef[B] = Map(f, this)
}
case class StringDef(header: String) extends DataDef[String]    
case class IntDef(headerName: String) extends DataDef[Int]
case class BigDecimalDef(headerName: String) extends DataDef[BigDecimal]
case class Pure[A](a: () => A) extends DataDef[A]
case class Map[A,B](f: A => B, dataDef: DataDef[A]) extends DataDef[B]
case class Combine[T<:Tuple,I<:Tuple.InverseMap[T,DataDef],B](f: I => B, dataDefs: T) extends DataDef[B]



object syntax {

    def constant[A](a: A) = Pure(() => a)

    def string(header: String) = 
        StringDef(header)

    def int(header: String) = 
        IntDef(header)

    def bigDecimal(header: String) =
        BigDecimalDef(header)  

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
    def combine[T<:Tuple,I<:Tuple.InverseMap[T,DataDef],B](f: I => B, t: T)(
        using mappedBy: AllDataDefs[t.type]
        ): Combine[T,I,B] = Combine(f, t)

}


