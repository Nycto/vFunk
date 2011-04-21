/**
 * Numeric comparison validators
 */

package main.scala.vfunk.validate

/**
 * A base class for validators that compare numeric values
 */
protected class NumericCompare (
    private val vs: Number,
    private val predicate: (Double, Number) => Boolean,
    private val err: Err
) extends Validator {
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

