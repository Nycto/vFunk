package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.validate._

@RunWith(classOf[JUnitSuiteRunner])
class PlumbingTests extends Specification with JUnit {

    "A Manual Validator" should {
        "Return no errors when given an empty list" in {
            val manual = new Manual

            manual.isValid("data") must_== true
            manual.getErrors("data") must_== Nil
        }

        "Return the errors it is given" in {
            val errors = Err("1", "one") :: Err("2", "two") :: Nil
            val manual = new Manual(errors)

            manual.isValid("data") must_== false
            manual.getErrors("data") must_== errors
        }
    }

}

