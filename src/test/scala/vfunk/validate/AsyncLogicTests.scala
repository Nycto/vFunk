package test.roundeights.vfunk.validate

import org.specs2.mutable._

import com.roundeights.vfunk.validate._
import com.roundeights.vfunk.Err

class ValidationAsyncLogicTests extends Specification {

    "An And Validator" should {
        "Pass when all its sub-validators pass" in {
            new AsyncAnd( new Manual, new Manual, new Manual ) must
                validateFor("data")
        }

        "Short circuit when the first sub-validator fails" in {
            val validator = new AsyncAnd(
                new Manual( Err("1", "One") ),
                new Manual( Err("2", "Two") )
            )

            validator must notValidateFor("data")
            validator.getErrors("data") must ===(Err("1", "One") :: Nil).await
        }

        "Pass when empty" in {
            new And() must validateFor("data")
        }

        "Fail when any of the sub-validators fail" in {
            val validator = new AsyncAnd(
                new Manual, new Manual,
                new Manual( Err("1", "One"), Err("2", "Two") )
            )

            validator must notValidateFor("data")
            validator.getErrors("data") must
                ===(Err("1", "One") :: Err("2", "Two") :: Nil).await
        }

        "Allow validators to be easily combined" in {
            (new Manual && new Manual && new Manual) must validateFor("data")
            (new Manual && new Manual &&
                new Manual(Err("1", "One"), Err("2", "Two"))) must
                notValidateFor("data")
        }
    }

    "An Or Validator" should {
        "Pass when all its sub-validators pass" in {
            new AsyncOr( new Manual, new Manual, new Manual ) must
                validateFor("data")
        }

        "Pass when any of its sub-validators pass" in {
            val validator = new AsyncOr(
                new Manual( Err("1", "One"), Err("2", "Two") ),
                new Manual,
                new Manual( Err("3", "Three"), Err("4", "Four") )
            )
            validator must validateFor("data")
        }

        "Pass when empty" in {
            new Or() must validateFor("data")
        }

        "Fail when all of its sub-validators fail" in {
            val validator = new AsyncOr(
                new Manual( Err("1", "One"), Err("2", "Two") ),
                new Manual( Err("3", "Three") )
            )
            validator must notValidateFor("data")
            validator.getErrors("data") must ===(
                Err("1", "One") :: Err("2", "Two") :: Err("3", "Three") :: Nil
            ).await
        }

        "Allow validators to be easily combined" in {
            (
                new Manual( Err("1", "One"), Err("2", "Two") ) ||
                new Manual ||
                new Manual( Err("3", "Three"), Err("4", "Four") )
            ) must validateFor("data")

            (
                new Manual( Err("1", "One"), Err("2", "Two") ) ||
                new Manual( Err("3", "Three") ) ||
                new Manual( Err("4", "For") )
            ) must notValidateFor("data")
        }
    }

    "A Not Validator" should {
        "Pass when its sub-validator fails" in {
            new AsyncNot( new Manual( Err("3", "Three") ) ) must
                validateFor("data")
        }

        "Fail when its sub-validator passes" in {
            new AsyncNot( new Manual, "Oops" ) must
                notValidateFor("data")
        }
    }

}

