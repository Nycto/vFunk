/**
 * Numeric comparison validators
 */

package main.scala.vfunk.validate

/**
 * A base class for validators that compare numeric values
 */
protected class NumericCompare (
    against: Number,
    private val predicate: (Double, Double) => Boolean,
    private val err: Err
) extends Validator {
    private val vs = against.doubleValue
    override def getErrors ( value: String ) = {
        try {
            predicate(value.toDouble, vs) match {
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
 * Validates whether two values are numerically equal
 */
class Equals ( vs: Number ) extends NumericCompare (
    vs, _ == _, Err("EQUALS", "Must equal " + vs)
) {}

/**
 * Validates that a value is less than a given
 */
class LessThan ( vs: Number ) extends NumericCompare (
    vs, _ < _, Err("LESSTHAN", "Must be less than " + vs)
) {}

/**
 * Validates that a value is less than or equal to a given
 */
class LessThanEquals ( vs: Number ) extends NumericCompare (
    vs, _ <= _, Err("LESSTHANEQUALS", "Must be less than or equal to " + vs)
) {}

