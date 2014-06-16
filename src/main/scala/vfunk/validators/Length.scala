package com.roundeights.vfunk.validate

import com.roundeights.vfunk.{Validator, Validated, Err}

/**
 * A helper class for validators that check the length of a string
 */
protected abstract class LengthValidator (
    private val length: Int,
    private val code: String,
    private val message: String,
    private val predicate: (Int, Int) => Boolean
) extends Validator {

    require( length >= 0, "Length must be greater than or equal to 0" )

    /** {@inheritDoc} */
    override def getErrors ( value: String ) = {
        Validated(
            predicate( value.length, length ),
            Err( code, message.format(length, if (length > 1) "s" else "") )
        )
    }
}

/**
 * Validates that the string is at least the given length
 */
class MinLength ( length: Int ) extends LengthValidator (
    length,
    "MINLENGTH",
    "Must be at least %d character%s long",
    ( actual: Int, vs: Int ) => actual >= vs
) {

    /** {@inheritDoc} */
    override def toString = "Validate(MinLength(%s))".format(length)
}


/**
 * Validates that the string is no longer than the given length
 */
class MaxLength ( length: Int ) extends LengthValidator (
    length,
    "MAXLENGTH",
    "Must not be longer than %d character%s",
    ( actual: Int, vs: Int ) => actual <= vs
) {

    /** {@inheritDoc} */
    override def toString = "Validate(MaxLength(%s))".format(length)
}

/**
 * Validates that the string is exactly the given length
 */
class ExactLength ( length: Int ) extends LengthValidator (
    length,
    "EXACTLENGTH",
    "Must be exactly %d character%s long",
    ( actual: Int, vs: Int ) => actual == vs
) {

    /** {@inheritDoc} */
    override def toString = "Validate(ExactLength(%s))".format(length)
}

/**
 * Validates that a string isn't empty
 */
class NotEmpty extends LengthValidator (
    0,
    "NOTEMPTY",
    "Must not be empty",
    ( actual: Int, vs: Int ) => actual > vs
) {

    /** {@inheritDoc} */
    override def toString = "Validate(NotEmpty)"
}

