package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.validate._

@RunWith(classOf[JUnitSuiteRunner])
class ValidationLogicTests extends Specification with JUnit {

    "An And Validator" should {
        "Pass when all its sub-validators pass" in {
            val validator = new And( new Manual, new Manual, new Manual )
            validator must validateFor("data")
        }
        "Short circuit when the first sub-validator fails" in {
            val validator = new And(
                new Manual( Err("1", "One") ),
                new Manual( Err("2", "Two") )
            )

            validator must notValidateFor("data")
            validator.getErrors("data") must_== Err("1", "One") :: Nil
        }
        "Pass when empty" in {
            val validator = new And
            validator must validateFor("data")
        }
        "Fail when any of the sub-validators fail" in {
            val validator = new And(
                new Manual, new Manual,
                new Manual( Err("1", "One"), Err("2", "Two") )
            )

            validator must notValidateFor("data")
            validator.getErrors("data") must_==
                Err("1", "One") :: Err("2", "Two") :: Nil
        }
    }

    "An Or Validator" should {
        "Pass when all its sub-validators pass" in {
            val validator = new Or( new Manual, new Manual, new Manual )
            validator must validateFor("data")
        }
        "Pass when any of its sub-validators pass" in {
            val validator = new Or(
                new Manual( Err("1", "One"), Err("2", "Two") ),
                new Manual,
                new Manual( Err("3", "Three"), Err("4", "Four") )
            )
            validator must validateFor("data")
        }
        "Pass when empty" in {
            val validator = new Or
            validator must validateFor("data")
        }
        "Fail when all of its sub-validators fail" in {
            val validator = new Or(
                new Manual( Err("1", "One"), Err("2", "Two") ),
                new Manual( Err("3", "Three") )
            )
            validator must notValidateFor("data")
            validator.getErrors("data") must_==
                Err("1", "One") :: Err("2", "Two") :: Err("3", "Three") :: Nil
        }
    }
    "A Not Validator" should {
        "Pass when its sub-validator fails" in {
            val validator = new Not( new Manual( Err("3", "Three") ) )
            validator must validateFor("data")
        }

        "Fail when its sub-validator passes" in {
            val validator = new Not( new Manual, "Oops" )
            validator must notValidateFor("data")
        }
    }
}

