package com.bones.tcsv

case class ErrorMessage(message: String) extends AnyVal

/** A non-empty list, _1 is the head, and _2 is the tail. */
type Errors = (ErrorMessage, List[ErrorMessage])
def error(str: String) = (ErrorMessage(str), List.empty)
def concatErrors(e1: Errors, e2: Errors) : Errors = {
    (e1._1, e1._2 ::: e2._1 :: e2._2)
}
/** Right[C] if both inputs are Right[_], otherwise accumulates errors.*/
def ap2[A,B,C](r1: Either[Errors,A], r2: Either[Errors,B], f: (A,B) => C): Either[Errors,C] = {
    (r1, r2) match {
        case (Left(e1), Left(e2)) => Left(concatErrors(e1,e2))
        case (Left(e), _) => Left(e)
        case (_, Left(e)) => Left(e)
        case (Right(a), Right(b)) => Right(f(a,b))
    }
}

extension(v: Vector[String]):
        def getOrError(index: Int): Either[Errors,String] = {
            if (index < v.length) Right(v(index))
            else Left( (ErrorMessage(s"Index ${index} is out of bounds, the Vector is of length: ${v.length}"), List.empty) )
        }

