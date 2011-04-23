/**
 * Validators that check the length of a string
 */

package main.scala.vfunk.validate

/**
 * A helper class for validators that check the length of a string
 */
protected abstract class LengthValidator ( private val length: Int )
    extends Validator {

    require( length >= 0, "Length must be greater than or equal to 0" )
    protected def predicate ( actual: Int, vs: Int ): Boolean
    protected val code: String
    protected val message: String

    override def getErrors ( value: String ) = {
        predicate( value.length, length ) match {
            case true => Nil
            case false => List(Err(
                code,
                message.format(
                    length,
                    if (length > 1) "s" else ""
                )
            ))
        }
    }
}

/**
 * Validates that the string is at least the given length
 */
class MinLength ( length: Int ) extends LengthValidator ( length ) {
    override protected def predicate ( actual: Int, vs: Int ) = actual >= vs
    protected lazy val code = "MINLENGTH"
    protected lazy val message = "Must be at least %d character%s long"
}

/**
 * Validates that the string is no longer than the given length
 */
class MaxLength ( length: Int ) extends LengthValidator ( length ) {
    override protected def predicate ( actual: Int, vs: Int ) = actual <= vs
    protected lazy val code = "MAXLENGTH"
    protected lazy val message = "Must not be longer than %d character%s"
}

/**
 * Validates that the string is exactly the given length
 */
class ExactLength ( length: Int ) extends LengthValidator ( length ) {
    override protected def predicate ( actual: Int, vs: Int ) = actual == vs
    protected lazy val code = "EXACTLENGTH"
    protected lazy val message = "Must be exactly %d character%s long"
}

/**
 * Validates that a string isn't empty
 */
class NotEmpty extends LengthValidator (0) {
    override protected def predicate ( actual: Int, vs: Int ) = actual > vs
    protected lazy val code = "NOTEMPTY"
    protected lazy val message = "Must not be empty"
}

