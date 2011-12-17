/**
 * Structural code for defining the Validation interface
 */

package com.roundeights.vfunk

/**
 * A validation error
 */
case class Err ( val code: String, val message: String )

/**
 * The result of a validation pass
 */
case class Validated ( val value: String, val errors: List[Err] ) {

    /**
     * Returns whether this value is valid
     */
    lazy val isValid: Boolean = errors.isEmpty

    /**
     * Returns the first error, if there is one
     */
    lazy val firstError: Option[Err] = errors.headOption
}

/**
 * Validates that a value matches a given rule
 */
trait Validator {

    /**
     * Returns the validation errors for a value
     */
    def getErrors ( value: String ): List[Err]

    /**
     * Validates a value and returns detailed results
     */
    def validate ( value: String ): Validated
        = Validated( value, getErrors(value) )

    /**
     * Returns whether a value is valid or not
     */
    def isValid ( value: String ): Boolean
        = validate( value ).isValid
}

