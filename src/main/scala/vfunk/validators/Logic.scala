package com.roundeights.vfunk.validate

import com.roundeights.vfunk.{Validator, Validated, Err}
import scala.concurrent.{Future, ExecutionContext}

/**
 * A validator that requires that all the validators it contains to pass
 *
 * This will short circuit after the first error is returned
 */
class And ( private val validators: List[Validator] ) extends Validator {

    /** Constructor for fluently building validators */
    def this ( validators: Validator* ) = this( validators.toList )

    /** {@inheritDoc */
    override def getErrors(value: String)(implicit ctx: ExecutionContext) = {

        def find ( remaining: List[Validator] ): Future[List[Err]] = {
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
class Or ( private val validators: List[Validator] ) extends Validator {

    /** Constructor for fluently building validators */
    def this ( validators: Validator* ) = this( validators.toList )

    /** {@inheritDoc */
    override def getErrors(value: String)(implicit ctx: ExecutionContext) = {

        def find (
            remaining: List[Validator], errs: List[Err]
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
class Not (
    private val validator: Validator,
    private val message: String = "Value failed validation"
) extends Validator {

    /** {@inheritDoc */
    override def getErrors(value: String)(implicit ctx: ExecutionContext) = {
        validator.isValid(value)
            .flatMap(valid => Validated(!valid, Err("NOT", message)))
    }

    /** {@inheritDoc} */
    override def toString = "Validate(Not(%s))".format( validator )
}

