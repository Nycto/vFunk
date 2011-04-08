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

/**
 * Invokes a callback as a validator
 */
class Invoke ( private val callback: (String) => Traversable[Err] )
    extends Validator {

    override def getErrors ( value: String ) = callback(value).toList
}

