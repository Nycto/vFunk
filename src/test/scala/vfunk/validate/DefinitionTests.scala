package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.validate._

@RunWith(classOf[JUnitSuiteRunner])
class DefinitionTests extends Specification with JUnit {

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
            val valid = new Validator {
                override def getErrors ( value: String ) = Nil
            }

            valid.isValid("something") must_== true
            valid.validate("something") must_== Result("something", Nil)
        }

        "return as valid when there are errors" in {
            val valid = new Validator {
                override def getErrors ( value: String )
                    = List(Err("test", "error"))
            }

            valid.isValid("something") must_== false
            valid.validate("something") must_==
                Result("something", List(Err("test", "error")))
        }

    }

}