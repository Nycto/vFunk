package com.roundeights.vfunk

import com.roundeights.vfunk.validate.Manual
import com.roundeights.vfunk.filter.Identity
import scala.concurrent.Future

/**
 * A field definition
 */
trait Field {

    /** Returns the name of this field */
    def name(): String

    /** The filteration and validation against this field */
    def process ( value: String ): FieldResult

    /** Requires that this field is valid */
    def require ( value: String ): FieldResult = {
        val validated = process(value)
        if ( !validated.isValid ) {
            throw new InvalidFormException( name() -> validated )
        }
        validated
    }
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
) extends Errable {

    /** Returns the name of the field */
    def name: String = field.name

    /** Adds a new error to this result */
    def +: ( err: Err ): FieldResult
        = FieldResult( field, original, value, err +: validated )

    /** {@inheritDoc} */
    def errors: Seq[Err] = validated.errors

    /** Generates an Either based on the validation of this field */
    def either: Either[Validated,String]
        = if (isValid) Right( value ) else Left( validated )

    /** Generates an Option based on the validation of this field */
    def option: Option[String] = if (isValid) Some(value) else None

    /** Presents this results as a future */
    def future: Future[String] = isValid match {
        case true => Future.successful( value )
        case false => Future.failed( new InvalidFormException(name -> this) )
    }
}


