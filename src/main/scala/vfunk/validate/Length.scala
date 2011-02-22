/**
 * Validators that check the length of a string
 */

package main.scala.vfunk.validate

/**
 * A helper class for validators that check the length of a string
 */
private object LengthValidator {

    /**
     * Asserts the given condition and formats an error if it doesn't pass
     */
    def check (
        length: Int, condition: => Boolean, code: String, message: String
    ) = {
        condition match {
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
class MinLength ( private val length: Int ) extends Validator {
    require( length >= 0, "Length must be greater than or equal to 0" )
    override def getErrors ( value: String ) = {
        LengthValidator.check(
            length,
            value.length >= length,
            "MINLENGTH",
            "Must be at least %d character%s long"
        )
    }
}

/**
 * Validates that the string is no longer than the given length
 */
class MaxLength ( private val length: Int ) extends Validator {
    require( length >= 0, "Length must be greater than or equal to 0" )
    override def getErrors ( value: String ) = {
        LengthValidator.check(
            length,
            value.length <= length,
            "MAXLENGTH",
            "Must not be longer than %d character%s"
        )
    }
}

/**
 * Validates that a string isn't empty
 */
class NotEmpty extends Validator {
    override def getErrors ( value: String ) = {
        ( value.length > 0 ) match {
            case true => Nil
            case false => List(Err("NOTEMPTY", "Must not be empty"))
        }
    }
}

