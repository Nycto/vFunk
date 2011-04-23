/**
 * Numeric validators
 */

package main.scala.vfunk.validate

/**
 * Validates that a value is a number
 */
class IsNumeric extends Validator {
    override def getErrors ( value: String ) = {
        try {
            value.toDouble
            Nil
        }
        catch {
            case e:NumberFormatException => List(
                Err("NUMERIC", "Must be a number")
            )
        }
    }
}

