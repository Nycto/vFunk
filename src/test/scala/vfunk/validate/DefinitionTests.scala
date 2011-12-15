package test.scala.vfunk.validate

import org.specs2.mutable._

import main.scala.vfunk.validate._

class ValidationDefinitionTests extends Specification {

    "A Result" should {

        "return as valid when there are no errors" in {
            val result = Result("something", Nil);

            result.isValid must_== true
            result.value must_== "something"
            result.errors must_== Nil
            result.firstError must_== None
        }

        "return as invalid when there are errors" in {
            val errors = Err("Code", "Message") :: Err("Code2", "Another") :: Nil
            val result = Result("something", errors);

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
                Result("something", List(Err("test", "error")))
        }

    }

}

