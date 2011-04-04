package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.validate._

@RunWith(classOf[JUnitSuiteRunner])
class LogicTests extends Specification with JUnit {

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

    "A Not Validator" should {

        "Pass when its sub-validator fails" in {
            val not = new Not(
                new Manual( Err("3", "Three") )
            )
            not.isValid("data") must_== true
            not.getErrors("data") must_== Nil
        }

        "Fail when its sub-validator passes" in {
            val not = new Not( new Manual, "Oops" )
            not.isValid("data") must_== false
            not.getErrors("data") must_== Err("NOT", "Oops") :: Nil
        }
    }

}

