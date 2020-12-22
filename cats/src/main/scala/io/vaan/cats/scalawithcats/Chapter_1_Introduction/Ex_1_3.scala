package io.vaan.cats.scalawithcats.Chapter_1_Introduction

import io.vaan.datatype.Cat

// https://www.scalawithcats.com/dist/scala-with-cats.html#exercise-printable-library
object Ex_1_3 {

  // typeclass
  trait Printable[A] {
    def format(a: A): String
  }

  // interface object
  object Printable {
    def format[A](input: A)(implicit p: Printable[A]): String =
      p.format(input)

    def print[A](input: A)(implicit p: Printable[A]): Unit =
      println(p.format(input))
  }

  // instances
  object PrintableInstances {
    implicit val stringPrintable: Printable[String] = (a: String) => a
    implicit val intPrintable: Printable[Int] = (a: Int) => a.toString

    implicit val catPrintable: Printable[Cat] = (a: Cat) => {
      val name = Printable.format(a.name)
      val age = Printable.format(a.age)
      val color = Printable.format(a.color)

      s"$name is a $age year-old $color cat."
    }
  }

  // interface syntax
  object PrintableSyntax {
    implicit class PrintableOps[A](a: A)(implicit p: Printable[A]) {
      def format: String = p.format(a)
      def print(): Unit = println(p.format(a))
    }
  }
}
