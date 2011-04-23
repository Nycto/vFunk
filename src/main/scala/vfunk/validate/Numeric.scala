/**
 * Numeric validators
 */

package main.scala.vfunk.validate

/**
 * A base validator for numeric tests
 */
protected trait NumericValidator extends Validator {
    protected def predicate(value: Double): Boolean
    protected val err: Err
    override def getErrors ( value: String ) = {
        try {
            predicate( value.toDouble ) match {
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
class IsNumeric extends NumericValidator {
    override protected def predicate(value: Double) = true
    protected lazy val err = Err("NUMERIC", "Must be a number")
}

/**
 * Validates that a value is a number
 */
class Odd extends NumericValidator {
    override protected def predicate(value: Double) = (value % 2).abs == 1
    protected lazy val err = Err("ODD", "Must be odd")
}

/**
 * Validates that a value is a number
 */
class Even extends NumericValidator {
    override protected def predicate(value: Double) = (value % 2).abs == 0
    protected lazy val err = Err("EVEN", "Must be even")
}

