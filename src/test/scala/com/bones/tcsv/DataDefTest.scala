package com.bones.tcsv

import org.junit.Test
import org.junit.Assert._
import com.bones.tcsv.syntax._
import com.bones.tcsv._

class DataDefTest {


    @Test def buildDataStrcutre(): Unit = {
        case class Person(name: String, age: Int)

        val basePerson = string(0) :: int(1) :: dnil

        val fPerson = (Person.apply _).tupled
        val personDef = combine( fPerson, ListDef(basePerson))

        personDef match {
            case MapDef(f, ListDef(Cons(head, Cons(tail, EmptyList)))) => {
                assertEquals(head, string(0))
                assertEquals(tail, int(1))
            }
            case x => fail(s"Not a match: ${x}")
        }

        val personWithId = int(2) :: personDef :: dnil

        val baseTrait = string(3) :: int(4) :: dnil

        val personAndTrait = personWithId ++ baseTrait

        personAndTrait match {
            case PlusPlus(Cons(int2, Cons(MapDef(f, ListDef(Cons(head, Cons(tail, EmptyList)))), EmptyList)), Cons(str3, Cons(int4, EmptyList))) => {
                assertEquals(head, string(0))
                assertEquals(tail, int(1))
                assertEquals(int2, int(2))
                assertEquals(str3, string(3))
                assertEquals(int4, int(4))
            }
            case x => fail(s"Not a PlusPlus: ${x}")
        }     
    }
}


