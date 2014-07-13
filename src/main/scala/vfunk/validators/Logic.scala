package com.roundeights.vfunk.validate

import com.roundeights.vfunk.{Validator, Validated, Err}

import scala.annotation.tailrec

/**
 * A validator that requires that all the validators it contains to pass
 *
 * This will short circuit after the first error is returned
 */
class And ( private val validators: List[Validator] ) extends Validator {

    /** Constructor for fluently building validators */
    def this ( validators: Validator* ) = this( validators.toList )

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {

        @tailrec
        def find ( remaining: List[Validator] ): List[Err] = {
            remaining match {
                case Nil => Nil
                case head :: tail => {
                    val errs = head.getErrors( value )
                    errs.isEmpty match {
                        case false => errs
                        case true => find( tail )
                    }
                }
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
    override def getErrors ( value: String ) = {

        @tailrec
        def find ( remaining: List[Validator], errs: List[Err] ): List[Err] = {
            remaining match {
                case Nil => errs
                case head :: tail => {
                    val currentErrs = head.getErrors( value )
                    currentErrs.isEmpty match {
                        case true => Nil
                        case false => find( tail, errs ::: currentErrs )
                    }
                }
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
    private val err: Err
) extends Validator {

    /** Instantiates from a synchronous validator */
    def this(
        validator: Validator,
        message: String = "Value failed validation"
    ) = this( validator, Err("NOT", message) )

    /** {@inheritDoc */
    override def getErrors ( value: String )
        = Validated(!validator.isValid(value), err)

    /** {@inheritDoc} */
    override def toString = "Validate(Not(%s))".format( validator )
}

