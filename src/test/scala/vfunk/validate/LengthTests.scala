package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.validate._

@RunWith(classOf[JUnitSuiteRunner])
class LengthTests extends Specification with JUnit {

    "A MinLength Validator" should {
        val minLength = new MinLength(5)

        "pass when a string is long enough" in {
            minLength.isValid("long enough") must_== true
            minLength.getErrors("long enough") must_== Nil
        }

        "fail when a string is too short" in {
            minLength.isValid("data") must_== false
            minLength.getErrors("data") must_!= Nil
        }
    }

    "A MaxLength Validator" should {
        val maxLength = new MaxLength(5)

        "pass when a string is short enough" in {
            maxLength.isValid("data") must_== true
            maxLength.getErrors("data") must_== Nil
        }

        "fail when a string is too long" in {
            maxLength.isValid("too long") must_== false
            maxLength.getErrors("too long") must_!= Nil
        }
    }

    "A NotEmpty Validator" should {
        val notEmpty = new NotEmpty

        "pass when a string is not empty" in {
            notEmpty.isValid("data") must_== true
            notEmpty.getErrors("data") must_== Nil
        }

        "fail when a string is empty" in {
            notEmpty.isValid("") must_== false
            notEmpty.getErrors("") must_!= Nil
        }
    }

}

