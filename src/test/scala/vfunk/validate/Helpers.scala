package test.scala.vfunk.validate

import main.scala.vfunk.validate._
import org.specs._
import org.specs.matcher._

/**
 * Companion that provides builder functionality
 */
object validateFor {
    def apply ( against: String* ) = new validateFor( against )
    def apply ( against: List[String] ) = new validateFor( against )
}

/**
 * A helper class for testing what a validator passes for
 */
class validateFor ( against: Seq[String] ) extends Matcher[Validator]() {
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
 * Companion that provides builder functionality
 */
object notValidateFor {
    def apply ( against: String* ) = new notValidateFor( against )
    def apply ( against: List[String] ) = new notValidateFor( against )
}

/**
 * A helper class for testing what a validator passes for
 */
class notValidateFor ( against: Seq[String] ) extends Matcher[Validator]() {
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

