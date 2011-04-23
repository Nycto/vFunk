/**
 * Numeric comparison validators
 */

package main.scala.vfunk.validate

/**
 * A base class for validators that compare numeric values
 */
protected abstract class NumericCompare ( against: Number )
    extends Validator {

    protected def predicate(actual: Double, vs: Double): Boolean
    protected val err: Err
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
class Equals ( vs: Number ) extends NumericCompare(vs) {
    override protected def predicate(actual: Double, vs: Double) = {
        actual == vs
    }
    protected lazy val err = Err("EQUALS", "Must equal " + vs)
}

/**
 * Validates that a value is less than a given
 */
class LessThan ( vs: Number ) extends NumericCompare (vs) {
    override protected def predicate(actual: Double, vs: Double) = {
        actual < vs
    }
    protected lazy val err = Err("LESSTHAN", "Must be less than " + vs)
}

/**
 * Validates that a value is less than or equal to a given
 */
class LessThanEquals ( vs: Number ) extends NumericCompare (vs) {
    override protected def predicate(actual: Double, vs: Double) = {
        actual <= vs
    }
    protected lazy val err = Err(
        "LESSTHANEQUALS", "Must be less than or equal to " + vs
    )
}

/**
 * Validates that a value is greater than a given
 */
class GreaterThan ( vs: Number ) extends NumericCompare (vs) {
    override protected def predicate(actual: Double, vs: Double) = {
        actual > vs
    }
    protected lazy val err = Err("GREATERTHAN", "Must be greater than " + vs)
}

/**
 * Validates that a value is greater than or equal to a given
 */
class GreaterThanEquals ( vs: Number ) extends NumericCompare (vs) {
    override protected def predicate(actual: Double, vs: Double) = {
        actual >= vs
    }
    protected lazy val err = Err(
        "GREATERTHANEQUALS", "Must be greater than or equal to " + vs
    )
}

