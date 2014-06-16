package com.roundeights.vfunk

import scala.concurrent.{Future, ExecutionContext}
import com.roundeights.vfunk.validate._
import scala.util.matching.Regex

/**
 * Builders for creating validators
 */
object Validate {

    def email = new Email
    def ipv4 = new IPv4
    def ipv6 = new IPv6
    def minLength ( length: Int ) = new MinLength( length )
    def maxLength ( length: Int ) = new MaxLength( length )
    def exactLength ( length: Int ) = new ExactLength( length )
    def length ( length: Int ) = exactLength( length )
    def notEmpty = new NotEmpty

    def and( validators: List[Validator] ) = new And( validators )
    def and( validators: Validator* ) = new And( validators:_* )

    def or( validators: List[Validator] ) = new Or( validators )
    def or( validators: Validator* ) = new Or( validators:_* )

    def not (
        validator: Validator,
        message: String = "Value failed validation"
    ) = new Not( validator, message )

    def isNumeric = new IsNumeric
    def odd = new Odd
    def even = new Even

    def equals ( vs: Number ) = new Equals( vs )
    def == ( vs: Number ) = new Equals( vs )

    def lessThan ( vs: Number ) = new LessThan( vs )
    def < ( vs: Number ) = new LessThan( vs )

    def lessThanEquals ( vs: Number ) = new LessThanEquals( vs )
    def <= ( vs: Number ) = new LessThanEquals( vs )

    def greaterThan ( vs: Number ) = new GreaterThan( vs )
    def > ( vs: Number ) = new GreaterThan( vs )

    def greaterThanEquals ( vs: Number ) = new GreaterThanEquals( vs )
    def >= ( vs: Number ) = new GreaterThanEquals( vs )

    def manual( errors: List[Err] ) = new Manual( errors )
    def manual( errors: Err* ) = new Manual( errors )
    def manual( code: String, message: String ) = new Manual(Err(code, message))

    def invoke (callback: (String) => Future[Traversable[Err]]): Validator
        = new Invoke(callback)
    def invokeList (callback: (String) => Traversable[Err]): Validator
        = invoke( value => Future.successful(callback(value)) )
    def invokeErr (callback: (String) => Err): Validator
        = invokeList( value => Seq(callback(value)) )
    def invokeTuple (callback: (String) => (String, String)): Validator
        = invokeErr( value => Err(callback(value)) )

    def in ( options: Set[String] ) = new In( options )
    def in ( options: String* ) = new In( options:_* )

    def alphaNum = new AlphaNum
    def alpha = new Alpha
    def digit = new Digit
    def hex = new Hex

    def chars( allowed: Set[Char] ) = new Characters( allowed )
    def chars( allowed: Seq[Char]* ) = new Characters( allowed:_* )
    def chars( allowed: String ) = new Characters( allowed )

    def same ( versus: String, caseSensitive: Boolean = false )
        = new Same(versus, caseSensitive)

    def noWhitespace = new NoWhitespace
    def regExp ( regex: Regex ) = new RegExp( regex )
    def regExp ( regex: String ) = new RegExp( regex )
    def notBlank = new NotBlank

    def contains( chars: Set[Char] ) = new Contains( chars )
    def contains( chars: Seq[Char]* ) = new Contains( chars:_* )
    def contains( chars: String ) = new Contains( chars )

    def errMessage( validator: Validator, error: String )
        = new ErrMessage(validator, error)
}

/** @see Err */
object Err {

    /** Creates an error from a tuple */
    def apply( tuple: (String, String) ): Err = new Err( tuple._1, tuple._2 )
}

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

    /** Returns the errors for this field */
    def errors: Seq[Err]

    /** Returns whether this field validated */
    def isValid: Boolean = errors.isEmpty

    /** Returns the first error */
    def firstError: Option[Err] = errors.headOption

    /** Returns the first error message */
    def firstMessage: Option[String] = firstError.map( _.message )

    /** Requires that this result be valid, otherwise throw an exception */
    def require: this.type = {
        if ( !isValid )
            throw InvalidValueException( this )
        this
    }
}

/** @see Validated */
object Validated {

    /** Creates a result from a validated value */
    def apply( result: Boolean, message: => Err ): Future[List[Err]]
        = Future.successful( if ( result ) Nil else List(message) )
}

/**
 * The result of a validation pass
 */
case class Validated (
    val value: String,
    override val errors: Seq[Err]
) extends Errable {

    /** Adds a new error to this result */
    def +: ( err: Err ): Validated = Validated(value, err +: errors)
}

/**
 * Validates that a value matches a given rule
 */
trait Validator {

    /** Returns the validation errors for a value */
    def getErrors
        ( value: String )
        ( implicit ctx: ExecutionContext )
    : Future[List[Err]]

    /** Validates a value and returns detailed results */
    def validate
        ( value: String )
        ( implicit ctx: ExecutionContext )
    : Future[Validated]
        = getErrors(value).map( Validated( value, _ ) )

    /** Returns whether a value is valid or not */
    def isValid
        ( value: String )
        ( implicit ctx: ExecutionContext )
    : Future[Boolean]
        = validate( value ).map( _.isValid )

    /** Wraps this validator in a custom error message */
    def message ( msg: String ): Validator = Validate.errMessage( this, msg )
}

