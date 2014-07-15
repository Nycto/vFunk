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
    def require ( value: String ): ValidFieldResult = process(value).require
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
    : Future[ValidFieldResult]
        = process(value).map(_.require)

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
 * Represents a field guaranteed to be valid
 */
abstract class CommonFieldResult(
    val name: String,
    val original: String
) {

    /** Returns the value of this field result */
    def value: String

    /** Generates an Either based on the validation of this field */
    def either: Either[Validated,String]

    /** Generates an Option based on the validation of this field */
    def option: Option[String]

    /** Presents this results as a future */
    def future: Future[String]

    /** Requires that this result be valid, otherwise throw an exception */
    def require: ValidFieldResult

    /** Returns this field as an Errable field result */
    def asFieldResult: FieldResult
}

/**
 * The results from an individual field
 */
case class FieldResult (
    fieldName: String,
    originalValue: String,
    val validated: Validated
) extends CommonFieldResult( fieldName, originalValue ) with Errable {

    /** Instantiates a field result that doesn't have any errors */
    def this( name: String, original: String, value: String )
        = this(name, original, Validated(value, Seq()))

    /** {@inheritDoc} */
    override def value = validated.value

    /** Adds a new error to this result */
    def +: ( err: Err ): FieldResult
        = FieldResult( name, original, err +: validated )

    /** {@inheritDoc} */
    override def errors: Seq[Err] = validated.errors

    /** {@inheritDoc} */
    override def either: Either[Validated,String]
        = if (isValid) Right( value ) else Left( validated )

    /** {@inheritDoc} */
    override def option: Option[String] = if (isValid) Some(value) else None

    /** {@inheritDoc} */
    override def future: Future[String] = isValid match {
        case true => Future.successful( value )
        case false => Future.failed( new InvalidFormException(name -> this) )
    }

    /** {@inheritDoc} */
    override def require: ValidFieldResult = {
        if ( !validated.isValid ) {
            throw new InvalidFormException( name -> this )
        }
        ValidFieldResult(name, originalValue, validated.value)
    }

    /** {@inheritDoc} */
    override def asFieldResult: FieldResult = this
}

/**
 * The results from an individual field
 */
case class ValidFieldResult (
    fieldName: String,
    originalValue: String,
    override val value: String
) extends CommonFieldResult( fieldName, originalValue ) {

    /** {@inheritDoc} */
    override def either: Either[Validated,String] = Right(value)

    /** {@inheritDoc} */
    override def option: Option[String] = Some(value)

    /** {@inheritDoc} */
    override def future: Future[String]
        = Future.successful( value )

    /** {@inheritDoc} */
    override def require: ValidFieldResult = this

    /** {@inheritDoc} */
    override def asFieldResult: FieldResult
        = new FieldResult( name, original, value )
}


