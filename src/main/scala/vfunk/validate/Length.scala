/**
 * Validators that check the length of a string
 */

package main.scala.vfunk.validate

/**
 * Validates that the string is at least the given length
 */
class MinLength ( private val length: Int ) extends Validator {
    require( length >= 0, "Length must be greater than or equal to 0" )
    override def getErrors ( value: String ) = {
        ( value.length >= length ) match {
            case true => Nil
            case false => List(Err(
                "MINLENGTH",
                "Must be at least %d character%s long".format(
                    length, if (length > 1) "s" else ""
                )
            ))
        }
    }
}

/**
 * Validates that the string is no longer than the given length
 */
class MaxLength ( private val length: Int ) extends Validator {
    require( length >= 0, "Length must be greater than or equal to 0" )
    override def getErrors ( value: String ) = {
        ( value.length <= length ) match {
            case true => Nil
            case false => List(Err(
                "MAXLENGTH",
                "Must not be longer than %d character%s".format(
                    length, if (length > 1) "s" else ""
                )
            ))
        }
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

