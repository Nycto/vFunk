/**
 * A collection of Validator implementations
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
        @tailrec def find ( remaining: List[Validator] ): List[Err] = {
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


