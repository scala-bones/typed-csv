package com.bones.tcsv

import scala.util.matching.Regex

trait Validation[A] {
    def isValid(input: A): Boolean
    def errorMessage(input: A): String
    def description: String
}

trait BaseValidation[A] {
    
    case class ValidValues(a: A*) {
        val valid = a.toSet
        def isValid(input: A) = valid.contains(input)
        def errorMessage(input: A) = "Not valid"
    }

}

object StringValidation {
    case class MatchesRegex(regex: Regex) extends Validation[String] {
        def isValid(input: String) = regex.matches(input)
        def errorMessage(input: String) = s"Input '$input' does not match the regular expression '${regex.pattern}'"
        def description = s"Input must match regular expression '${regex.pattern}'"
    }
}