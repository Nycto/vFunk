package com.roundeights.vfunk

import com.roundeights.vfunk.validate.Manual
import com.roundeights.vfunk.filter.Identity
import scala.concurrent.{Future, ExecutionContext}

/**
 * Methods shared between different types of fields
 */
trait CommonField {

    /** Returns the name of this field */
    def name: String

    /** Returns the filter applied to this field */
    def filter: Filter

    /** Converts this field into an async field */
    def async: AsyncField
}

/**
 * A field definition
 */
trait Field extends CommonField {

    /** Returns the validator applied to this field */
    def validator: Validator

    /** Sets values in this field */
    def set(
        name: String = this.name,
        filter: Filter = this.filter,
        validator: Validator = this.validator
    ): Field

    /** 'Ands' another validator into this field */
    def andValidator ( toAdd: Validator ): Field
        = set( validator = Validate.and(validator, toAdd) )

    /** 'Ands' another validator into this field */
    def andValidator ( callback: (String) => Traversable[Err] ): Field
        = andValidator( Validate.invoke(callback) )

    /** 'Ands' another validator into this field */
    def andFilter ( toAdd: Filter ): Field
        = set( filter = Filter.chain(filter, toAdd) )

    /** 'Ands' another validator into this field */
    def andFilter ( callback: (String) => String ): Field
        = andFilter( Filter.callback(callback) )

    /** The filteration and validation against this field */
    def process ( value: String ): FieldResult

    /** Requires that this field is valid */
    def require ( value: String ): FieldResult = {
        val validated = process(value)
        if ( !validated.isValid ) {
            throw new InvalidFormException( name -> validated )
        }
        validated
    }
}

/**
 * A text field
 */
case class TextField (
    override val name: String,
    override val filter: Filter = new Identity,
    override val validator: Validator = new Manual
) extends Field {

    /** {@inheritDoc} */
    override def set(
        name: String = this.name,
        filter: Filter = this.filter,
        validator: Validator = this.validator
    ): Field = {
        TextField( name, filter, validator )
    }

    /** {@inheritDoc} */
    override def process ( value: String ): FieldResult = {
        val filtered = filter.filter( value )
        FieldResult( name, value, validator.validate(filtered) )
    }

    /** {@inheritDoc} */
    override def async: AsyncField
        = AsyncTextField( name, filter, validator.async )
}

/**
 * A asyncrhonous field
 */
trait AsyncField extends CommonField {

    /** Returns the validator applied to this field */
    def validator: AsyncValidator

    /** Sets values in this field */
    def set(
        name: String = this.name,
        filter: Filter = this.filter,
        validator: AsyncValidator = this.validator
    ): AsyncField

    /** Runs the filteration and validation against this field */
    def process
        ( value: String )
        ( implicit ctx: ExecutionContext )
    : Future[FieldResult]

    /** Requires that this field is valid, failing the future if it isn't */
    def require
        ( value: String )
        ( implicit ctx: ExecutionContext )
    : Future[FieldResult] = {
        process(value).map(validated => {
            if ( !validated.isValid ) {
                throw new InvalidFormException( name -> validated )
            }
            validated
        })
    }

    /** Presents the validated and filtered result as a future */
    def value
        ( value: String )
        ( implicit ctx: ExecutionContext )
    : Future[String]
        = this.require(value).map(_.value)
}


/**
 * A text field
 */
case class AsyncTextField (
    override val name: String,
    override val filter: Filter = new Identity,
    override val validator: AsyncValidator = (new Manual).async
) extends AsyncField {

    /** {@inheritDoc} */
    override def set(
        name: String = this.name,
        filter: Filter = this.filter,
        validator: AsyncValidator = this.validator
    ): AsyncField = {
        AsyncTextField( name, filter, validator )
    }

    /** {@inheritDoc} */
    override def process
        ( value: String )
        ( implicit ctx: ExecutionContext )
    : Future[FieldResult] = {
        validator
            .validate(filter.filter(value))
            .map(FieldResult(name, value, _))
    }

    /** {@inheritDoc} */
    override def async: AsyncField = this
}


/**
 * The results from an individual field
 */
case class FieldResult (
    val name: String,
    val original: String,
    val validated: Validated
) extends Errable {

    /** Returns the value of this field result */
    def value = validated.value

    /** Adds a new error to this result */
    def +: ( err: Err ): FieldResult
        = FieldResult( name, original, err +: validated )

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


