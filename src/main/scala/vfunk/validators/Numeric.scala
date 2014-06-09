package com.roundeights.vfunk.validate

import com.roundeights.vfunk.{Validator, Err}
import scala.concurrent.{Future, ExecutionContext}
import scala.util.Try

/**
 * A base validator for numeric tests
 */
protected abstract class NumericValidator (
    private val err: Err,
    private val predicate: (Double) => Boolean
) extends Validator {

    /** {@inheritDoc */
    override def getErrors(value: String)(implicit ctx: ExecutionContext) = {
        Future.fromTry(Try {
            try {
                if ( predicate(value.toDouble) ) {
                    Nil
                } else {
                    List(err)
                }
            }
            catch {
                case e: NumberFormatException => List(err)
            }
        })
    }
}

/**
 * Validates that a value is a number
 */
class IsNumeric extends NumericValidator (
    Err("NUMERIC", "Must be a number"),
    (value: Double) => true
) {
    /** {@inheritDoc} */
    override def toString = "Validate(IsNumeric)"
}

/**
 * Validates that a value is a number
 */
class Odd extends NumericValidator (
    Err("ODD", "Must be odd"),
    (value: Double) => (value % 2).abs == 1
) {
    /** {@inheritDoc} */
    override def toString = "Validate(Odd)"
}

/**
 * Validates that a value is a number
 */
class Even extends NumericValidator (
    Err("EVEN", "Must be even"),
    (value: Double) => (value % 2).abs == 0
) {
    /** {@inheritDoc} */
    override def toString = "Validate(Even)"
}

