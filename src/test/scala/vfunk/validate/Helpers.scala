package test.scala.vfunk.validate

import main.scala.vfunk.validate._
import org.specs._
import org.specs.matcher._

/**
 * A helper class for testing what a validator passes for
 */
case class validateFor ( against: String* ) extends Matcher[Validator]() {
    def apply ( actual: => Validator ) = {
        val mismatch = against.foldRight [Option[String]] ( None ) {
            (versus, error) => {
                actual.isValid( versus ) match {
                    case false => Some("pass for string: " + versus)
                    case true => error
                }
            }
        }

        mismatch match {
            case Some(error) => (false, "", error)
            case None => (true, "validation passed as expected", "")
        }
    }
}

/**
 * A helper class for testing what a validator passes for
 */
case class notValidateFor ( against: String* ) extends Matcher[Validator]() {
    def apply ( actual: => Validator ) = {
        val mismatch = against.foldRight [Option[String]] ( None ) {
            (versus, error) => {
                actual.isValid( versus ) match {
                    case false => error
                    case true => Some("fail for string: " + versus)
                }
            }
        }

        mismatch match {
            case Some(error) => (false, "", error)
            case None => (true, "validation failed as expected", "")
        }
    }
}

