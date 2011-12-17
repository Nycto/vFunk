/**
 * Numeric validators
 */

package com.roundeights.vfunk.validate

import com.roundeights.vfunk.{Validator, Err}

/**
 * A base validator for numeric tests
 */
protected abstract class NumericValidator (
    private val err: Err,
    private val predicate: (Double) => Boolean
) extends Validator {

    /** {@inheritDoc */
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
class IsNumeric extends NumericValidator (
    Err("NUMERIC", "Must be a number"),
    (value: Double) => true
)

/**
 * Validates that a value is a number
 */
class Odd extends NumericValidator (
    Err("ODD", "Must be odd"),
    (value: Double) => (value % 2).abs == 1
)

/**
 * Validates that a value is a number
 */
class Even extends NumericValidator (
    Err("EVEN", "Must be even"),
    (value: Double) => (value % 2).abs == 0
)

