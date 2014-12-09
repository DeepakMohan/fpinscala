package exceptionhandling

import org.scalatest.FunSuite

/**
 * Created by dmohan200 on 12/4/14 4:28 PM.
 */
class EitherTest extends FunSuite {

  import Either._

  test("Either: mean") {
    assert(Right[Double](17d) == mean(List(5d, 10d, 15d, 25d, 30d)))
    assert(Left("mean of empty list") == mean(List()))
  }

  test("EXERCISE 4.6: map") {
    assert(Right(10) == Right(5).map(_ * 2))
    val e: Either[String, Int] = Left("five")
    assert(Left("five") == e.map(_ * 2))
  }

  test("EXERCISE 4.6: flatMap") {
    assert(Right(25) == Right(5).flatMap(i => if(i < 0) Left("input must be non-negative") else Right(i * i)))
    assert(Left("input must be non-negative") == Right(-5).flatMap(i => if(i < 0) Left("input must be non-negative") else Right(i * i)))

    val e: Either[String, Int] = Left("invalid input")
    assert(Left("invalid input") == e.flatMap(i => Right(i * 10)))
  }

  test("EXERCISE 4.6: orElse") {
    assert(Right(100) == Left("hundred").orElse(Right(100)))
    assert(Right(99) == Right(99).orElse(Right(100)))
  }

  test("ECERCISE 4.6: map2") {

    def parseMass(mass: String): Either[Exception, Double] = {
      if(mass.endsWith("lb")) {
        Try(mass.replace("lb", "").toDouble * 0.45359237)
      } else { //simply assume kg
        Try(mass.replace("kg", "").toDouble)
      }
    }

    def parseHeight(h: String): Either[Exception, Double] = {
      if(h.endsWith("inches"))
        Try(h.replace("inches", "").toDouble * 0.0254)
      else
        Try(h.replace("m", "").toDouble)
    }

    def bmi(mass: Double, height: Double): Double = mass/(Math.pow(height, 2))

    assert(Right(3189.3067901362137) == parseMass("60kg").map2(parseHeight("0.13716m"))(bmi))
    assert(Right(3189.3067826395404) == parseMass("132.277357lb").map2(parseHeight("5.4inches"))(bmi))
    assert(Right(3189.3067901362137) == Try("60".toDouble).map2(Try("0.13716".toDouble))(bmi))
    assert(Right(3189.3067901362137) == Right(60d).map2(Right(0.13716d))(bmi))

    def sq(a: Int, b: Int): Int = a * b
    assert(Right(25) == Right(5).map2(Right(5))(sq))
    assert(Left("five") == Left("five").map2(Right(5))(sq))
    assert(Left("five") == Right(5).map2(Left("five"))(sq))

    assert(Right(25) == Right(5).map2_1(Right(5))(sq))
    assert(Left("five") == Left("five").map2_1(Right(5))(sq))
    assert(Left("five") == Right(5).map2_1(Left("five"))(sq))

    assert(Right(25) == Right(5).map2_2(Right(5))(sq))
    assert(Left("five") == Left("five").map2_2(Right(5d))(bmi))
    assert(Left("five") == Right(5d).map2_2(Left("five"))(bmi))
  }

  test("EXERCISE 4.7 sequence") {
    assert(Right(List[Int](1,2,3,4)) == sequence(List[Either[String, Int]](Right(1), Right(2), Right(3), Right(4))))
    assert(Right(List()) == sequence(List[Either[String, Int]]()))
    assert(Left("three") == sequence(List[Either[String, Int]](Right(1), Right(2), Left("three"), Left("four"), Right(5), Right(6))))
  }

  test("EXERCISE 4.7 traverse") {
    assert(Right(List[Int](1,4,9,16)) == traverse(List[Either[String, Int]](Right(1), Right(2), Right(3), Right(4)))(a => a * a))
    assert(Right(List()) == traverse(List[Either[String, Int]]())(a => a * a))
    assert(Left("three") == traverse(List[Either[String, Int]](Right(1), Right(2), Left("three"), Left("four"), Right(5), Right(6)))(a => a * a))
  }

  case class Person(name: Name, age: Age) {
    override def equals(o: Any): Boolean = {
      o.isInstanceOf[Person] && this.name.value == o.asInstanceOf[Person].name.value && this.age.value == o.asInstanceOf[Person].age.value
    }

  }
  sealed class Name(n: String) {
    def value = n
  }
  sealed class Age(a: Int) {
    def value = a
  }

  def name(n: String): Either[String, Name] = if(n == null || n == "") Left("Name is empty") else Right(new Name(n))
  def age(a: Int): Either[String, Age] = if(a < 0) Left("invalid age") else Right(new Age(a))
  def person(n: String, a: Int): Either[String, Person] = name(n).map2(age(a))(Person(_, _))

  assert(Right(Person(new Name("deepak"), new Age(30))) == person("deepak", 30))
  assert(Left("Name is empty") == person("", 30))
  assert(Left("invalid age") == person("deepak", -1))
}
