package test.roundeights.vfunk.validate

import org.specs2.mutable._

import com.roundeights.vfunk._

class ValidationDefinitionTests extends Specification {

    "A Validated" should {

        val errors = Err("Code", "Message") :: Err("Code2", "Another") :: Nil
        val invalid = Validated("something", errors);
        val valid = Validated("something", Nil);

        "return as valid when there are no errors" in {
            valid.isValid must_== true
            valid.value must_== "something"
            valid.errors must_== Nil
            valid.firstError must_== None
        }

        "return as invalid when there are errors" in {
            invalid.isValid must_== false
            invalid.value must_== "something"
            invalid.errors must_== errors
            invalid.firstError must_== Some( Err("Code", "Message") )
        }

        "not throw an exception when a required value validates" in {
            valid.require
            ok
        }

        "throw an exception when a required value does not validate" in {
            invalid.require must throwAn[InvalidValueException]
        }

    }

    "A Validator" should {

        "return as valid when there are errors" in {
            val validator = new Validator {
                override def getErrors ( value: String ) = Nil
            }

            validator must validateFor("something")
        }

        "return as valid when there are errors" in {
            val validator = new Validator {
                override def getErrors ( value: String )
                    = List(Err("test", "error"))
            }

            validator must notValidateFor("something")

            validator.validate("something") must_==
                Validated("something", List(Err("test", "error")))
        }

    }

}

