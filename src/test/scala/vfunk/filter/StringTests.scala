package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.filter._

@RunWith(classOf[JUnitSuiteRunner])
class FilterStringTests extends Specification with JUnit {

   "A Trim filter" should {
        val filter = new Trim
        "Leave a string without whitespace unchanged" in {
            filter.filter("data") must_== "data"
        }
        "Remove whitespace from both sides of a string" in {
            filter.filter("  data   ") must_== "data"
            filter.filter("data   ") must_== "data"
            filter.filter("   data") must_== "data"
            filter.filter("\n\r\tdata\n\r\t") must_== "data"
        }
    }
   "An AlphaNum filter" should {
        val filter = new AlphaNum
        "Leave a string with only alphanumeric characters unchanged" in {
            val data = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
                "abcdefghijklmnopqrstuvwxyz" +
                "1234567890";

            filter.filter(data) must_== data
        }
        "Strip non-alphanumeric characters from a string" in {
            val data = ((0 to 47) ++ (58 to 64) ++ (91 to 96) ++ (127 to 255))
                .map( _.asInstanceOf[Char] )
                .foldLeft("")(_ + _)

            filter.filter(data) must_== ""
        }
    }
}

