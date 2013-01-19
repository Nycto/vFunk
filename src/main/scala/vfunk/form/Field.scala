package com.roundeights.vfunk

import com.roundeights.vfunk.validate.Manual
import com.roundeights.vfunk.filter.Identity

/**
 * A field definition
 */
trait Field {

    /**
     * Returns the name of this field
     */
    def name(): String

    /**
     * The filteration and validation against this field
     */
    def process ( value: String ): FieldResult

}

/**
 * A text field
 */
case class TextField (
    val name: String,
    val filter: Filter = new Identity,
    val validator: Validator = new Manual
) extends Field {

    /**
     * Runs validation against this form
     */
    override def process ( value: String ): FieldResult = {
        val filtered = filter.filter( value )
        FieldResult( this, value, filtered, validator.validate(filtered) )
    }

}

/**
 * The results from an individual field
 */
case class FieldResult (
    val field: Field,
    val original: String,
    val value: String,
    val validated: Validated
) {

    /**
     * Returns the name of the field
     */
    def name: String = field.name

    /**
     * Returns whether this field validated
     */
    def isValid: Boolean = validated.isValid

    /**
     * Returns the errors for this field
     */
    def errors: List[Err] = validated.errors

    /**
     * Returns the first error
     */
    def firstError: Option[Err] = validated.firstError

    /**
     * Returns the first error message
     */
    def firstMessage: Option[String] = firstError.map( _.message )

    /**
     * Requires that this result be valid, otherwise throw an exception
     */
    def require: FieldResult = {
        validated.require
        this
    }

    /**
     * Generates an Either based on the validation of this field
     */
    def either: Either[Validated,String] = isValid match {
        case true => Right( value )
        case false => Left( validated )
    }

    /**
     * Generates an Option based on the validation of this field
     */
    def option: Option[String] = isValid match {
        case true => Some( value )
        case false => None
    }

}


