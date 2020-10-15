package com.bones.tcsv

import org.junit.Test
import org.junit.Assert._
import com.bones.tcsv.syntax._
import com.bones.tcsv._

class DataDefTest {


    @Test def buildDataStrcutre(): Unit = {
        case class Person(name: String, age: Int, weight: BigDecimal)

        val basePerson = (string("Name"), int("Age"), bigDecimal("Weight"))

        val fPerson = (Person.apply _).tupled
        val personDef = combine( fPerson, basePerson)

        personDef match {
            case Combine(f, defs) => {
                assertEquals(defs.head, string("Name"))
                assertEquals(defs.tail.head, int("Age"))
                assertEquals(defs.tail.tail.head, bigDecimal("Weight"))
                // defs.tail.tail.tail.head // <-- fails to compile as expected
            }
        }

        val personWithId = ( int("id"),personDef )

        val baseTrait = (string("HairColor"), bigDecimal("ShoeSize"))

        val personAndTrait = personWithId ++ baseTrait

        val expectedPersonAndTrait = 
            (IntDef("id"), 
                Combine(fPerson, (StringDef("Name"), IntDef("Age"), BigDecimalDef("Weight"))), 
                StringDef("HairColor"), 
                BigDecimalDef("ShoeSize")
            )
        assertEquals( personAndTrait, expectedPersonAndTrait)

        def isEquivalent[A<:Tuple, B<:Tuple](using A =:= B) = true        
        assertTrue( isEquivalent[Tuple.InverseMap[expectedPersonAndTrait.type,DataDef],(Int, Person, String, BigDecimal)] )

    }
}


