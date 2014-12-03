package exceptionhandling

import org.scalatest.FunSuite

/**
 * Created by dmohan200 on 12/3/14 8:58 AM.
 */
class OptionTest extends FunSuite {

  import Option._

  test("Option map") {
    val res: Option[Int] = Some(5).map {v => v * v}
    assert(Some(25) == res)
  }

  test("Option flatMap") {
    assert(Some(25) == Some(5).flatMap {v => Some(v * v)})
    assert(None == None.flatMap {v:Int => Some(v * v)})
    val num: Option[Int] = None
    assert(None == num.flatMap {v => Some(v * v)})

    assert(Some(6) == Some("deepak").flatMap(a => Some(a.length)))
    val unnamed: Option[String] = None
    assert(None == unnamed.flatMap(a => Some(a.length)))
  }

  test("Option getOrElse") {
    assert("deepak" == Some("deepak").getOrElse("buvana"))
    assert("buvana" == None.getOrElse("buvana"))
  }

  test("Option orElse") {
    assert(Some("deepak") == Some("deepak").orElse(Some("buvana")))
    assert(Some("buvana") == None.orElse(Some("buvana")))
  }

  test("Option filter") {
    assert(Some(252) == Some(252).filter(_%2 == 0))
    assert(None == Some(251).filter(_%2 == 0))
  }

  test("arithmetic mean") {
    val v = mean(Seq(1d,5d,10d,15d,20d,25d,30d))
    assert(Some(15.142857142857142) == v)

    assert(None == mean(Seq()))
  }

  test("EXERCISE 4.2: Variance") {
    val v = variance(Seq(1d,5d,10d,15d,20d,25d,30d))
    assert(Some(95.83673469387756) == v)

    assert(None == variance(Seq()))
  }

  test("EXERCISE 4.4: sequence") {
    assert(None == sequence(List(None)))
    assert(Some(List("deepak")) == sequence(List(Some("deepak"))))

    assert(None == sequence(List[Option[String]](Some("deepak"), None)))
    assert(None == sequence(List[Option[String]](Some("deepak"), None, Some("buvana"))))
    assert(None == sequence(List[Option[String]](None, Some("deepak"), Some("buvana"))))
    assert(Some(List("deepak", "buvana", "kavinaya")) == sequence(List[Option[String]](Some("deepak"), Some("buvana"), Some("kavinaya"))))
  }

  test("EXERCISE 4.4: sequence2") {
    assert(None == sequence2(List(None)))
    assert(Some(List("deepak")) == sequence2(List(Some("deepak"))))

    assert(None == sequence2(List[Option[String]](Some("deepak"), None)))
    assert(None == sequence2(List[Option[String]](Some("deepak"), None, Some("buvana"))))
    assert(None == sequence2(List[Option[String]](None, Some("deepak"), Some("buvana"))))
    assert(Some(List("deepak", "buvana", "kavinaya")) == sequence2(List[Option[String]](Some("deepak"), Some("buvana"), Some("kavinaya"))))
  }

  test("EXERCISE 4.4: sequence3") {
    assert(None == sequence1(List(None)))
    assert(Some(List("deepak")) == sequence1(List(Some("deepak"))))

    assert(None == sequence1(List[Option[String]](Some("deepak"), None)))
    assert(None == sequence1(List[Option[String]](Some("deepak"), None, Some("buvana"))))
    assert(None == sequence1(List[Option[String]](None, Some("deepak"), Some("buvana"))))
    assert(Some(List("deepak", "buvana", "kavinaya")) == sequence1(List[Option[String]](Some("deepak"), Some("buvana"), Some("kavinaya"))))
  }

  test("parseInts") {
    assert(Some(List(1,2,3)) == parseInts(List("1", "2", "3")))
    assert(None == parseInts(List("1", "2", "3", "four", "five")))
  }

  test("EXERCISE 4.5 traverse") {
    assert(Some(List(1,2,3)) == traverse(List("1", "2", "3"))(i => Try(i.toInt)))
    assert(None == traverse(List("1", "2", "3", "four", "five"))(i => Try(i.toInt)))
  }

}
