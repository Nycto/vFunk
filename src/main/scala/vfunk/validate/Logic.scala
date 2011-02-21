/**
 * Boolean Logic oriented validators
 */

package main.scala.vfunk.validate

import scala.annotation.tailrec

/**
 * Returns the given errors
 */
class Manual ( private val errors: Traversable[Err] ) extends Validator {
    def this ( errors: Err* ) = this( errors )
    override def getErrors ( value: String ) = errors.toList
}

/**
 * A validator that requires that all the validators it contains to pass
 *
 * This will short circuit after the first error is returned
 */
class And ( private val validators: List[Validator] ) extends Validator {

    def this ( validators: Validator* ) = this( validators.toList )

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

    def this ( validators: Validator* ) = this( validators.toList )

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

