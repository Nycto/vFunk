package com.roundeights.vfunk.validate

import com.roundeights.vfunk.{AsyncValidator, CommonValidator, Validator}
import com.roundeights.vfunk.{Validated, Err}
import scala.concurrent.{Future, ExecutionContext}

/**
 * A validator that requires that all the validators it contains to pass
 *
 * This will short circuit after the first error is returned
 */
class AsyncAnd (
    private val validators: List[AsyncValidator]
) extends AsyncValidator {

    /** Constructor for fluently building validators */
    def this ( validators: CommonValidator[_]* )
        = this( validators.map(_.async).toList )

    /** {@inheritDoc} */
    override def getErrors(value: String)(implicit ctx: ExecutionContext) = {

        def find ( remaining: List[AsyncValidator] ): Future[List[Err]] = {
            remaining match {
                case Nil => Future.successful(Nil)
                case head :: tail => head.getErrors(value).flatMap(_ match {
                    case Nil => find(tail)
                    case errs => Future.successful(errs)
                })
            }
        }

        find( validators )
    }

    /** {@inheritDoc} */
    override def toString
        = "Validate(And(%s))".format( validators.mkString(", ") )
}

/**
 * A validator that requires any of its sub-validators to pass
 */
class AsyncOr (
    private val validators: List[AsyncValidator]
) extends AsyncValidator {

    /** Constructor for fluently building validators */
    def this ( validators: CommonValidator[_]* )
        = this( validators.map(_.async).toList )

    /** {@inheritDoc} */
    override def getErrors(value: String)(implicit ctx: ExecutionContext) = {

        def find (
            remaining: List[AsyncValidator], errs: List[Err]
        ): Future[List[Err]] = {
            remaining match {
                case Nil => Future.successful(errs)
                case head :: tail => head.getErrors( value ).flatMap(_ match {
                    case Nil => Future.successful(Nil)
                    case currentErrs => find( tail, errs ::: currentErrs )
                })
            }
        }

        find( validators, Nil )
    }

    /** {@inheritDoc} */
    override def toString
        = "Validate(Or(%s))".format( validators.mkString(", ") )
}

/**
 * A validator that inverts the results of a contained validator
 */
class AsyncNot (
    validate: CommonValidator[_],
    private val err: Err
) extends AsyncValidator {

    /** Instantiates from a synchronous validator */
    def this( validator: CommonValidator[_], message: String )
        = this( validator, Err("NOT", message) )

    /** Instantiates from a synchronous validator */
    def this( validator: CommonValidator[_] )
        = this( validator,  "Value failed validation" )

    /** The internal validator */
    private val validator = validate.async

    /** {@inheritDoc} */
    override def getErrors(value: String)(implicit ctx: ExecutionContext) = {
        validator.isValid(value).map(valid => Validated(!valid, err))
    }

    /** {@inheritDoc} */
    override def toString = "Validate(Not(%s))".format( validator )
}

