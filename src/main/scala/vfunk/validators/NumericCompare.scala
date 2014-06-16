package com.roundeights.vfunk.validate

import com.roundeights.vfunk.{Validator, Err}

/**
 * A base class for validators that compare numeric values
 */
protected abstract class NumericCompare (
    against: Number,
    err: Err,
    predicate: (Double, Double) => Boolean
) extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        try {
            predicate(value.toDouble, against.doubleValue) match {
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
class Equals ( vs: Number ) extends NumericCompare(
    vs,
    Err("EQUALS", "Must equal " + vs),
    (actual: Double, vs: Double) => actual == vs
) {
    /** {@inheritDoc} */
    override def toString = "Validate(== %s)".format( vs )
}

/**
 * Validates that a value is less than a given
 */
class LessThan ( vs: Number ) extends NumericCompare (
    vs,
    Err("LESSTHAN", "Must be less than " + vs),
    (actual: Double, vs: Double) => actual < vs
) {
    /** {@inheritDoc} */
    override def toString = "Validate(< %s)".format( vs )
}

/**
 * Validates that a value is less than or equal to a given
 */
class LessThanEquals ( vs: Number ) extends NumericCompare (
    vs,
    Err("LESSTHANEQUALS", "Must be less than or equal to " + vs),
    (actual: Double, vs: Double) => actual <= vs
) {
    /** {@inheritDoc} */
    override def toString = "Validate(<= %s)".format( vs )
}

/**
 * Validates that a value is greater than a given
 */
class GreaterThan ( vs: Number ) extends NumericCompare (
    vs,
    Err("GREATERTHAN", "Must be greater than " + vs),
    (actual: Double, vs: Double) => actual > vs
) {
    /** {@inheritDoc} */
    override def toString = "Validate(> %s)".format( vs )
}

/**
 * Validates that a value is greater than or equal to a given
 */
class GreaterThanEquals ( vs: Number ) extends NumericCompare (
    vs,
    Err("GREATERTHANEQUALS", "Must be greater than or equal to " + vs),
    (actual: Double, vs: Double) => actual >= vs
) {
    /** {@inheritDoc} */
    override def toString = "Validate(>= %s)".format( vs )
}

