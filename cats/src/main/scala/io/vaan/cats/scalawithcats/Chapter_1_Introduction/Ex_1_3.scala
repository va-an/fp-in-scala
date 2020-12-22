package io.vaan.cats.scalawithcats.Chapter_1_Introduction

import io.vaan.datatype.Cat

// https://www.scalawithcats.com/dist/scala-with-cats.html#exercise-printable-library
object Ex_1_3 {

  trait Printable[A] {
    def format(a: A): String
  }

  object Printable {
    implicit val stringPrintable: Printable[String] = (a: String) => a
    implicit val intPrintable: Printable[Int] = (a: Int) => a.toString

    implicit val catPrintable: Printable[Cat] = (a: Cat) => {
      val name = stringPrintable.format(a.name)
      val age = intPrintable.format(a.age)
      val color = stringPrintable.format(a.color)

      s"$name is a $age year-old $color cat."
    }
  }

  object PrintableSyntax {
    implicit class PrintableOps[A](a: A)(implicit p: Printable[A]) {
      def format: String = p.format(a)
      def print(): Unit = println(p.format(a))
    }
  }
}
