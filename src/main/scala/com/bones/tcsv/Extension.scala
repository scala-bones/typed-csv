package com.bones.tcsv

extension(v: Vector[String]):
    def getOrError(index: Int): Either[Errors,String] = {
        if (index < v.length) Right(v(index))
        else Left( (ErrorMessage(s"Index ${index} is out of bounds, the Vector is of length: ${v.length}"), List.empty) )
    }

