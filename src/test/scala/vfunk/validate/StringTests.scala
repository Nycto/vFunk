package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.validate._

@RunWith(classOf[JUnitSuiteRunner])
class StringTests extends Specification with JUnit {

    "An AlphaNum validator" should {

        val alphanum = new AlphaNum

        val valid = List(
            "string", "test123", "abc123XYZ",
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
            "abcdefghijklmnopqrstuvwxyz",
            "0123456789"
        )

        valid.foreach { (versus) => {
            ("pass for string: " + versus) in {
                alphanum.isValid(versus) must_== true
                alphanum.getErrors(versus) must_== Nil
            }
        }}

        val invalid = List(
            "!\"#$%&'()*+,-/:;<=>?@[\\]^`{|}~",
            "Has Some Spaces"
        )

        invalid.foreach { (versus) => {
            ("fail for string: " + versus) in {
                alphanum.isValid(versus) must_== false
                alphanum.getErrors(versus) must_!= Nil
            }
        }}
    }

    "An Alpha validator" should {

        val alpha = new Alpha

        val valid = List(
            "string", "abcXYZ", "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
            "abcdefghijklmnopqrstuvwxyz"
        )

        valid.foreach { (versus) => {
            ("pass for string: " + versus) in {
                alpha.isValid(versus) must_== true
                alpha.getErrors(versus) must_== Nil
            }
        }}

        val invalid = List(
            "test123", "!\"#$%&'()*+,-/:;<=>?@[\\]^`{|}~",
            "Has Some Spaces", "0123456789"
        )

        invalid.foreach { (versus) => {
            ("fail for string: " + versus) in {
                alpha.isValid(versus) must_== false
                alpha.getErrors(versus) must_!= Nil
            }
        }}
    }

    "A Digit validator" should {

        val digit = new Digit

        "pass for string: 0123456789" in {
            digit.isValid("0123456789") must_== true
            digit.getErrors("0123456789") must_== Nil
        }

        val invalid = List(
            "test123", "!\"#$%&'()*+,-/:;<=>?@[\\]^`{|}~",
            "Has Some Spaces", "abcdefghijklmnopqrstuvwxyz"
        )

        invalid.foreach { (versus) => {
            ("fail for string: " + versus) in {
                digit.isValid(versus) must_== false
                digit.getErrors(versus) must_!= Nil
            }
        }}
    }

    "An Equals Validator" should {

        "Pass when strings are exactly equal" in {
            val equals = new Equals("Some String")
            equals.isValid("Some String") must_== true
            equals.getErrors("Some String") must_== Nil
        }

        "Pass for the default case-sensitive comparison" in {
            val equals = new Equals("Some String")
            equals.isValid("some string") must_== true
            equals.getErrors("some string") must_== Nil
        }

        "Fail when the strings are different" in {
            val equals = new Equals("Some String")
            equals.isValid("another string") must_== false
            equals.getErrors("another string") must_!= Nil
        }

        "Pass when strings are exactly equal for case-sensitive checks" in {
            val equals = new Equals("Some String", true)
            equals.isValid("Some String") must_== true
            equals.getErrors("Some String") must_== Nil
        }

        "Fail when case-sensitivity is flagged" in {
            val equals = new Equals("Some String", true)
            equals.isValid("some String") must_== false
            equals.getErrors("some String") must_!= Nil
        }
    }

    "A Whitespace validator" should {

        val whitespace = new NoWhitespace

        val valid = List( "string", "test123", "abc123XYZ" )

        valid.foreach { (versus) => {
            ("pass for string: " + versus) in {
                whitespace.isValid(versus) must_== true
                whitespace.getErrors(versus) must_== Nil
            }
        }}

        val invalid = List(
            "Has\nSome\nNew\nLines",
            "Has\rCarriage\rReturns",
            "Has Some Spaces"
        )

        invalid.foreach { (versus) => {
            ("fail for string: " + versus) in {
                whitespace.isValid(versus) must_== false
                whitespace.getErrors(versus) must_!= Nil
            }
        }}
    }

    "A RegExp validator" should {
      val regexp = new RegExp("simple");

      "Pass when the expression matches" in {
          regexp.isValid("simple") must_== true
          regexp.getErrors("simple") must_== Nil
      }

      "Fail when the expression doesn't match" in {
          regexp.isValid("oops") must_== false
          regexp.getErrors("oops") must_!= Nil
      }
    }

    "A NotBlank validator" should {

        val notBlank = new NotBlank

        val valid = List( " This is a string ", "Another\nOne" )
        valid.foreach { (versus) => {
            ("pass for string: " + versus) in {
                notBlank.isValid(versus) must_== true
                notBlank.getErrors(versus) must_== Nil
            }
        }}

        val invalid = List( "", "   ", "\n\r\t" )
        invalid.foreach { (versus) => {
            ("fail for string: " + versus) in {
                notBlank.isValid(versus) must_== false
                notBlank.getErrors(versus) must_!= Nil
            }
        }}
    }

}

