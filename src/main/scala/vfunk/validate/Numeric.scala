/**
 * Numeric validators
 */

package main.scala.vfunk.validate

protected class NumericValidator (
    private val test: (Double) => Boolean, err: Err
) extends Validator {
     override def getErrors ( value: String ) = {
        try {
            test( value.toDouble ) match {
                case true => Nil
                case false => List(err)
            }
        }
        catch {
            case e:NumberFormatException => List(err)
        }
    }
}

/**
 * Validates that a value is a number
 */
class IsNumeric extends NumericValidator (
    _ => true, Err("NUMERIC", "Must be a number")
) {}

/**
 * Validates that a value is a number
 */
class Odd extends NumericValidator (
    value => (value % 2).abs == 1, Err("ODD", "Must be odd")
) {}

