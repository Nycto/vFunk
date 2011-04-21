package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.validate._

@RunWith(classOf[JUnitSuiteRunner])
class ValidationNumericTests extends Specification with JUnit {

    "An Equals validator" should {
        "Properly compare integers" in {
            val validator = new Equals(5)
            validator must validateFor("5")
            validator must notValidateFor(
                "6", "4.99999", "5.00001", "abc123"
            )
        }
        "Properly compare floats" in {
            val validator = new Equals(3.1415)
            validator must validateFor("3.1415")
            validator must notValidateFor(
                "3", "3.1416", "3.141", "abc123"
            )
        }

    }
}
