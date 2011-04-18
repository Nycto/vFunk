package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import scala.collection.immutable.HashSet

import main.scala.vfunk.filter._

@RunWith(classOf[JUnitSuiteRunner])
class FilterStringTests extends Specification with JUnit {

    /**
     * A helper method that builds a string from a list of characters
     */
    private def build( list: Range* ) = {
        list.foldLeft[Traversable[Int]] ( Nil ) ( _ ++ _ )
            .map( _.asInstanceOf[Char] )
            .foldLeft("")(_ + _)
    }

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
            val data = build(0 to 47, 58 to 64, 91 to 96, 123 to 255)
            filter.filter(data) must_== ""
        }
    }
    "An Alpha filter" should {
        val filter = new Alpha
        "Leave a string with only alphabetic characters unchanged" in {
            val data = "ABCDEFGHIJKLMNOPQRSTUVWXYZbcdefghijklmnopqrstuvwxyz"
            filter.filter(data) must_== data
        }
        "Strip non-alphabetic characters from a string" in {
            val data = build(0 to 64, 91 to 96, 123 to 255)
            filter.filter(data) must_== ""
        }
    }
    "A Digit filter" should {
        val filter = new Digit
        "Leave a string with only digit characters unchanged" in {
            val data = "0123456789"
            filter.filter(data) must_== data
        }
        "Strip non-digit characters from a string" in {
            val data = build(0 to 47, 58 to 255)
            filter.filter(data) must_== ""
        }
    }
    "A Printable filter" should {
        val filter = new Printable
        "Leave a string with only printable characters unchanged" in {
            val data = build(32 to 126)
            filter.filter(data) must_== data
        }
        "Strip non-printable characters from a string" in {
            val data = build(0 to 31, 127 to 255)
            filter.filter(data) must_== ""
        }
    }
    "A Characters filter" should {
        "Leave a string with only valid characters unchanged" in {
            val filter = new Characters(HashSet('a', 'b', 'c', 'd'))
            filter.filter("dbac") must_== "dbac"
        }
        "Remove invalid characters from a string" in {
            val filter = new Characters("Valid")
            filter.filter("This is a string") must_== "iiai"
        }
        "Be constructable with a list of characters" in {
            val filter = new Characters('v', 'a', 'l', 'i', 'd')
            filter.filter("This is a string") must_== "iiai"
        }
        "Be constructable with a range of characters" in {
            val filter = new Characters('a' to 'm')
            filter.filter("This is a string") must_== "hiiaig"
        }
    }
}

