/**
 * Validators covering the plumbing
 */

package main.scala.vfunk.validate

/**
 * Returns the given errors
 */
class Manual ( private val errors: Traversable[Err] ) extends Validator {
    def this ( errors: Err* ) = this( errors )
    override def getErrors ( value: String ) = errors.toList
}

