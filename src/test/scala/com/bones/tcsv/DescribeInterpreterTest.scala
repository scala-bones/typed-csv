package com.bones.tcsv

import org.junit.Test
import org.junit.Assert._
import com.bones.tcsv.syntax._
import com.bones.tcsv._
import DescribeInterpreter.Description
import com.bones.tcsv.DataDef._
import com.bones.tcsv.DataList._


class DescribeInterpreterTest {

    @Test def testDescribeString: Unit = {
        assertEquals(DescribeInterpreter.describe(string(0)), List(Description("Expect index 0 to be of type String")))
    }
    @Test def testDescribeInt: Unit = {
        assertEquals(DescribeInterpreter.describe(int(0)), List(Description("Expect index 0 to be of type Int")))
    }
    @Test def testDescribePure: Unit = {
        assertEquals(DescribeInterpreter.describe(PureDef("")), List.empty)
    }

    @Test def testDescribeList : Unit = {
        case class Person(name: String, age: Int)
        val basePerson = string(0) :: int(1) :: dnil

        val fPerson = (Person.apply _).tupled
        val personDef = combine( fPerson, ListDef(basePerson))
        val personWithId = int(2) :: personDef :: dnil

        val baseTrait = string(3) :: int(4) :: dnil

        val personAndTrait = personWithId ++ baseTrait

        val result = DescribeInterpreter.describeList(personAndTrait).map(_.description)
        
        val expectedResult = List(
            "Expect index 0 to be of type String",
            "Expect index 1 to be of type Int",
            "Expect index 2 to be of type Int",
            "Expect index 3 to be of type String",
            "Expect index 4 to be of type Int"
        )

        assertEquals(result, expectedResult)

    }
}