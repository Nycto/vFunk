package test.scala.vfunk.validate

import org.specs2.mutable._

import main.scala.vfunk.validate._

class ValidationPlumbingTests extends Specification {

    "A Manual Validator" should {
        "Return no errors when given an empty list" in {
            val validator = new Manual
            validator must validateFor("data")
        }
        "Return the errors it is given" in {
            val errors = Err("1", "one") :: Err("2", "two") :: Nil
            val validator = new Manual(errors)

            validator must notValidateFor("data")
            validator.getErrors("data") must_== errors
        }
    }

    "An Invoke Validator" should {
        "Pass when the callback returns no errors" in {
            val validator = new Invoke( _ => Nil )
            validator must validateFor("data")
        }
        "Fail when the callback returns an error" in {
            val validator = new Invoke( _ => List(Err("1", "one")) )
            validator must notValidateFor("data")
        }
    }

    "An In Validator" should {
        "Pass when the set contains the value" in {
            val validator = new In( "One", "Two", "Three" )
            validator must validateFor("One")
        }
        "Fail when the set doesn't contain the value" in {
            val validator = new In( Set("One", "Two", "Three") )
            validator must notValidateFor("Nope")
        }
    }

}

