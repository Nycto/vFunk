package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.validate._

@RunWith(classOf[JUnitSuiteRunner])
class ValidatorsTests extends Specification with JUnit {

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

    "An And Validator" should {

        "Pass when all its sub-validators pass" in {
            val and = new And( new Manual, new Manual, new Manual )
            and.isValid("data") must_== true
            and.getErrors("data") must_== Nil
        }

        "Short circuit when the first sub-validator fails" in {
            val and = new And(
                new Manual( Err("1", "One") ),
                new Manual( Err("2", "Two") )
            )

            and.isValid("data") must_== false
            and.getErrors("data") must_== Err("1", "One") :: Nil
        }

        "Fail when any of the sub-validators fail" in {
            val and = new And(
                new Manual, new Manual,
                new Manual( Err("1", "One"), Err("2", "Two") )
            )

            and.isValid("data") must_== false
            and.getErrors("data") must_==
                Err("1", "One") :: Err("2", "Two") :: Nil
        }

    }

}

