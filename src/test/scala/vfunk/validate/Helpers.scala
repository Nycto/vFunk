package test.roundeights.vfunk.validate

import com.roundeights.vfunk._

import org.specs2.mutable._
import org.specs2.matcher._
import scala.concurrent.ExecutionContext
import scala.concurrent._
import scala.concurrent.duration._

/**
 * Companion that provides builder functionality
 */
object validateFor {
    def apply ( against: String* )( implicit ctx: ExecutionContext )
        = new validateFor( against )
    def apply ( against: List[String] )( implicit ctx: ExecutionContext )
        = new validateFor( against )
}

/**
 * A helper class for testing what a validator passes for
 */
class validateFor
    ( against: Seq[String] )
    ( implicit ctx: ExecutionContext )
extends Matcher[Validator]() {

    /** {@inheritDoc} */
    override def apply[S <: Validator]
        (actual: Expectable[S])
    : MatchResult[S] = {

        val mismatch = against.foldRight[Option[String]] ( None ) {
            (versus, error) => {
                Await.result(
                    actual.value.isValid(versus),
                    Duration(1, "second")
                ) match {
                    case false => Some("pass for string: " + versus)
                    case true => error
                }
            }
        }

        mismatch match {
            case Some(error) => result(false, "", error, actual)
            case None => result(
                true, "validation passed as expected", "", actual
            )
        }
    }
}

/**
 * Companion that provides builder functionality
 */
object notValidateFor {
    def apply ( against: String* )( implicit ctx: ExecutionContext )
        = new notValidateFor( against )
    def apply ( against: List[String] )( implicit ctx: ExecutionContext )
        = new notValidateFor( against )
}

/**
 * A helper class for testing what a validator passes for
 */
class notValidateFor
    ( against: Seq[String] )
    ( implicit ctx: ExecutionContext )
extends Matcher[Validator]() {

    /** {@inheritDoc} */
    override def apply[S <: Validator]
        (actual: Expectable[S])
    : MatchResult[S] = {
        val mismatch = against.foldRight [Option[String]] ( None ) {
            (versus, error) => {
                Await.result(
                    actual.value.isValid(versus),
                    Duration(1, "second")
                ) match {
                    case false => error
                    case true => Some("fail for string: " + versus)
                }
            }
        }

        mismatch match {
            case Some(error) => result(false, "", error, actual)
            case None => result(
                true, "validation failed as expected", "", actual
            )
        }
    }
}

