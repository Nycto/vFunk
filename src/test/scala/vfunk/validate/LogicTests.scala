package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.validate._

@RunWith(classOf[JUnitSuiteRunner])
class LogicTests extends Specification with JUnit {

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

        "Pass when empty" in {
            val and = new And
            and.isValid("data") must_== true
            and.getErrors("data") must_== Nil
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

    "An Or Validator" should {

        "Pass when all its sub-validators pass" in {
            val or = new Or( new Manual, new Manual, new Manual )
            or.isValid("data") must_== true
            or.getErrors("data") must_== Nil
        }

        "Pass when any of its sub-validators pass" in {
            val or = new Or(
                new Manual( Err("1", "One"), Err("2", "Two") ),
                new Manual,
                new Manual( Err("3", "Three"), Err("4", "Four") )
            )
            or.isValid("data") must_== true
            or.getErrors("data") must_== Nil
        }

        "Pass when empty" in {
            val or = new Or
            or.isValid("data") must_== true
            or.getErrors("data") must_== Nil
        }

        "Fail when all of its sub-validators fail" in {
            val or = new Or(
                new Manual( Err("1", "One"), Err("2", "Two") ),
                new Manual( Err("3", "Three") )
            )
            or.isValid("data") must_== false
            or.getErrors("data") must_==
                Err("1", "One") :: Err("2", "Two") :: Err("3", "Three") :: Nil
        }
    }

}

