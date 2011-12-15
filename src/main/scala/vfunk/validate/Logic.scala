/**
 * Boolean Logic oriented validators
 */

package main.scala.vfunk.validate

import scala.annotation.tailrec

/**
 * A validator that requires that all the validators it contains to pass
 *
 * This will short circuit after the first error is returned
 */
class And ( private val validators: List[Validator] ) extends Validator {

    /**
     * Constructor for fluently building validators
     */
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

}

/**
 * A validator that requires any of its sub-validators to pass
 */
class Or ( private val validators: List[Validator] ) extends Validator {

    /**
     * Constructor for fluently building validators
     */
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
}

/**
 * A validator that inverts the results of a contained validator
 */
class Not (
    private val validator: Validator,
    private val message: String = "Value failed validation"
) extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        validator.isValid( value ) match {
            case false => Nil
            case true => List( Err("NOT", message) )
        }
    }

}

