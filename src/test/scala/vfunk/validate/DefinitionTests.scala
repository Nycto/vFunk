package test.roundeights.vfunk.validate

import org.specs2.mutable._

import com.roundeights.vfunk._

class ValidationDefinitionTests extends Specification {

    "A Validated" should {

        "return as valid when there are no errors" in {
            val result = Validated("something", Nil);

            result.isValid must_== true
            result.value must_== "something"
            result.errors must_== Nil
            result.firstError must_== None
        }

        "return as invalid when there are errors" in {
            val errors
                = Err("Code", "Message") :: Err("Code2", "Another") :: Nil
            val result = Validated("something", errors);

            result.isValid must_== false
            result.value must_== "something"
            result.errors must_== errors
            result.firstError must_== Some( Err("Code", "Message") )
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

