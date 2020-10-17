package com.bones.tcsv

import org.junit.Test
import org.junit.Assert._
import com.bones.tcsv.syntax._
import com.bones.tcsv._

class VectorInterpreterTest {


    @Test def testIntSuccessInterpreter(): Unit = {
        val result = VectorInterpreter.extract(IntDef(0))(RowValues(Vector("0")))
        assertEquals(result, Right(0))
    }
    @Test def testIntFailInterpreter(): Unit = {
        val result = VectorInterpreter.extract(IntDef(0))(RowValues(Vector("zero")))
        assertEquals(result, Left((ErrorMessage("Value zero could not be converted to an Int"), List.empty)))
    }
    @Test def testStrSuccessInterpreter(): Unit = {
        val result = VectorInterpreter.extract(StringDef(0))(RowValues(Vector("zero")))
        assertEquals(result, Right("zero"))
    }
    @Test def testListSuccess(): Unit = {
        val list = Cons(StringDef(0), Cons(IntDef(1), EmptyList))
        val result = VectorInterpreter.extractDefList(list)(RowValues(Vector("zero", "1")))
        assertEquals(result, Right( ("zero", 1)))
    }
    @Test def testListFail(): Unit = {
        val list = Cons(StringDef(0), Cons(IntDef(1), EmptyList))
        val result = VectorInterpreter.extractDefList(list)(RowValues(Vector("zero", "one")))
        assertEquals(result, Left((ErrorMessage("Value one could not be converted to an Int"), List.empty)))
    }
    @Test def testListFail2(): Unit = {
        val list = Cons(IntDef(0), Cons(IntDef(1), EmptyList))
        val result = VectorInterpreter.extractDefList(list)(RowValues(Vector("zero", "one")))
        assertEquals(result, Left((ErrorMessage("Value zero could not be converted to an Int"), List(ErrorMessage("Value one could not be converted to an Int")))))
    }
    @Test def testMap(): Unit = {
        case class Person(name: String, age: Int)
        val list = Cons(StringDef(0), Cons(IntDef(1), EmptyList))
        val map = MapDef( (Person.apply _).tupled, ListDef(list))
        val result = VectorInterpreter.extract(map)(RowValues(Vector("George Carlin", "42")))
        assertEquals(result, Right(Person("George Carlin", 42)))
    }

}