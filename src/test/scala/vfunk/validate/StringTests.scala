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

}

