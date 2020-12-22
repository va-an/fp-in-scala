package io.vaan.cats.scalawithcats.Chapter_1_Introduction

import io.vaan.cats.scalawithcats.Chapter_1_Introduction.Ex_1_3.Printable
import io.vaan.cats.scalawithcats.Chapter_1_Introduction.Ex_1_3.PrintableInstances._
import io.vaan.cats.scalawithcats.Chapter_1_Introduction.Ex_1_3.PrintableSyntax._
import io.vaan.datatype.Cat
import org.scalatest.flatspec.AnyFlatSpec

class Tests extends AnyFlatSpec {
  it should "ex 1.3 printable library" in {
    val barsik = Cat("Barsik", 4, "white")
    val expected = "Barsik is a 4 year-old white cat."

    assert(barsik.format == expected)
    assert(Printable.format(barsik) == expected)

    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
      barsik.print()
      assert(stream.toString.contains(barsik.format))
    }
  }
}
