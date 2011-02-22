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

