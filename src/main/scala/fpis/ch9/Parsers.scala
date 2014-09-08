package fpis.ch9

import fpis.ch8.{Gen, Prop}


trait Parsers[ParseError, Parser[+ _]] {
  self =>

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)

    def unitLaw[A](in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(succeed(1))(s) == Right(1))

    // ex 2
    def productLaw[A](p: Parser[A])(in: Gen[String]): Prop
  }


  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  implicit def string(s: String): Parser[String]


  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def product[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def zeroChars(c: Char): Parser[Int]

  def zero[A, B](implicit f: A => B): Parser[B]

  def zeroOrMoreChars(c: Char): Parser[Int] = char(c).many.slice.map(_.size)

  def oneOrMoreChars(c: Char): Parser[Int] = char(c).many1.slice.map(_.size)

  def zeroOrMoreFollowedByOneOrMoreChars(c1: Char, c2: Char): Parser[(Int, Int)] =
    product(zeroOrMoreChars(c1), oneOrMoreChars(c2))

  def many[A](p: Parser[A]): Parser[List[A]]

  def many1[A](p: Parser[A]): Parser[List[A]]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]

  // ex 1
  def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    p.flatMap(a => p2.map(b => f(a, b)))

  //  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = f(a)

  implicit def asParser[A, B](a: A)(implicit f: A => Parser[B]): ParserOps[B] = f(a)

  implicit case class ParserOps[A](p: Parser[A]) extends AnyVal {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def **[B](p2: Parser[B]) = self.product(p, p2)

    def product[B](p2: Parser[B]) = self.product(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def slice = self.slice(p)
  }

}
