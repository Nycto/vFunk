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

}

