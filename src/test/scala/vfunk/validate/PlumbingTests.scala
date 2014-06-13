package test.roundeights.vfunk.validate

import org.specs2.mutable._

import com.roundeights.vfunk.Validate
import com.roundeights.vfunk.validate._
import com.roundeights.vfunk.Err

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
            validator.getErrors("data") must ===(errors).await
        }
    }

    "An Invoke Validator" should {
        "Pass when the callback returns no errors" in {
            val validator = Validate.invokeList( _ => Nil )
            validator must validateFor("data")
        }
        "Fail when the callback returns an error" in {
            val validator = Validate.invokeErr( _ => Err("1", "one") )
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

    "An ErrMessage validator" should {

        "Pass when an inner validator passes" in {
            val validator = new ErrMessage( new Manual, "..." )
            validator must validateFor("One")
        }

        "Fail with the new message when the inner validator fails" in {
            val validator = new ErrMessage( new Manual(Err("12", "!")), "Err!" )
            validator.getErrors("Value") must ===(List(Err("12", "Err!"))).await
        }

        "Fail with the first error code when the inner validator fails" in {
            val errors = Err("1", "one") :: Err("2", "two") :: Nil
            val validator = new ErrMessage( new Manual(errors), "Err!" )
            validator.getErrors("Value") must ===(List(Err("1", "Err!"))).await
        }
    }

}

