package com.bones.tcsv

enum DataDef[A] {
    case StringDef(index: Int) extends DataDef[String]    
    case IntDef(index: Int) extends DataDef[Int]
    case PureDef[A](a: A) extends DataDef[A]
/**
    * @tparam I This represends the Input type of the Function.  This can either be a specific type
    *   such a `String` or it could be a Tuple of Types such as (String,Int)
    * @tparam B This is our expected result type after combining the inputs, eg: Person ( case class Person(name: String, age: Int))
    * @param f A function responsible for converting I to B with no side affects.
    * @param dataDefs The definition for the I type.
    */ 
    case MapDef[I,B](
        f: I => B, 
        dataDef: DataDef[I]) extends DataDef[B]
    case ListDef[T<:Tuple](list: DataList[T]) extends DataDef[T]    
}




enum DataList[A<:Tuple] {
    import DataDef._
    def ::[A](dataDef: DataDef[A]) = Cons(dataDef, this)
    def ++[H<:Tuple](head: DataList[H]) = PlusPlus(this, head)
    def asDef = ListDef(this)
    case EmptyList extends DataList[EmptyTuple]
    case Cons[A,T<:Tuple](dataDef: DataDef[A], tail: DataList[T]) extends DataList[A*:T]
    case PlusPlus[H<:Tuple,T<:Tuple](head: DataList[H], tail: DataList[T]) extends DataList[Tuple.Concat[H,T]]
}



object syntax {

    val dnil = DataList.EmptyList

    def constant[A](a: A) = DataDef.PureDef(() => a)

    def string(index: Int) = 
        DataDef.StringDef(index)

    def int(index: Int) = 
        DataDef.IntDef(index)

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
    def combine[T<:Tuple,B](f: T => B, t: DataDef[T]): DataDef[B] = DataDef.MapDef(f, t)


}

