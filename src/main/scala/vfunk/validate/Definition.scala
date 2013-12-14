/**
 * Structural code for defining the Validation interface
 */

package com.roundeights.vfunk

/**
 * A validation error
 */
case class Err ( val code: String, val message: String )


/** The base class for validation exceptions */
trait ValidationException extends Exception with Errable {

    /** {@inheritDoc} */
    override def getMessage: String = {
        errors.map(_.message).mkString(", ") match {
            case "" => "Invalid"
            case msg => msg
        }
    }
}


/**
 * Thrown when a required validation does not pass
 */
case class InvalidValueException (
    val validated: Errable
) extends ValidationException {

    /** Construct an exception from a list of errors */
    def this ( errs: Seq[Err] ) = this(new Errable {
        override def errors: Seq[Err] = errs
    })

    /** {@inheritDoc} */
    override def errors: Seq[Err] = validated.errors

    /** {@inheritDoc} */
    override def toString = "InvalidValueException(%s)".format( errors )
}

/**
 * An interface for objects that can contain errors
 */
trait Errable {

    /**
     * Returns the errors for this field
     */
    def errors: Seq[Err]

    /**
     * Returns whether this field validated
     */
    def isValid: Boolean = errors.isEmpty

    /**
     * Returns the first error
     */
    def firstError: Option[Err] = errors.headOption

    /**
     * Returns the first error message
     */
    def firstMessage: Option[String] = firstError.map( _.message )

    /**
     * Requires that this result be valid, otherwise throw an exception
     */
    def require: this.type = {
        if ( !isValid )
            throw InvalidValueException( this )
        this
    }

}

/**
 * The result of a validation pass
 */
case class Validated (
    val value: String,
    override val errors: Seq[Err]
) extends Errable

/**
 * Validates that a value matches a given rule
 */
trait Validator {

    /** Returns the validation errors for a value */
    def getErrors ( value: String ): List[Err]

    /** Validates a value and returns detailed results */
    def validate ( value: String ): Validated
        = Validated( value, getErrors(value) )

    /** Returns whether a value is valid or not */
    def isValid ( value: String ): Boolean
        = validate( value ).isValid

    /** Wraps this validator in a custom error message */
    def message ( msg: String ) = Validate.errMessage( this, msg )
}

