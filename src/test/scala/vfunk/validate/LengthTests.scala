package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.validate._

@RunWith(classOf[JUnitSuiteRunner])
class LengthTests extends Specification with JUnit {

    "A MinLength Validator" should {
        val validator = new MinLength(5)
        "pass when a string is long enough" in {
            validator must validateFor("long enough")
        }
        "fail when a string is too short" in {
            validator must notValidateFor("data");
        }
    }

    "A MaxLength Validator" should {
        val validator = new MaxLength(5)
        "pass when a string is short enough" in {
            validator must validateFor("data")
        }
        "fail when a string is too long" in {
            validator must notValidateFor("too long")
        }
    }

    "An ExactLength Validator" should {
        val validator = new ExactLength(4)
        "pass when a string is the right length" in {
            validator must validateFor("data")
        }
        "fail when a string not the right length" in {
            validator must notValidateFor("some data")
        }
    }

    "A NotEmpty Validator" should {
        val validator = new NotEmpty
        "pass when a string is not empty" in {
            validator must validateFor("data")
        }
        "fail when a string is empty" in {
            validator must notValidateFor("")
        }
    }
}

