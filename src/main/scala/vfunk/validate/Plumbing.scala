/**
 * Validators covering the plumbing
 */

package main.scala.vfunk.validate

/**
 * Returns the given errors
 */
class Manual ( private val errors: Traversable[Err] ) extends Validator {

    /**
     * Alternate constructor for more fluently creating a validator
     */
    def this ( errors: Err* ) = this( errors )

    /** {@inheritDoc */
    override def getErrors ( value: String ) = errors.toList

}

/**
 * Invokes a callback as a validator
 */
class Invoke (
    private val callback: (String) => Traversable[Err]
) extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = callback(value).toList

}

/**
 * Checks to see if a value is contained within a Set
 */
class In ( private val options: Set[String] ) extends Validator {

    /**
     * Alternate constructor for creating a validator from a list of strings
     */
    def this ( options: String* ) = this( options.toSet )

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        options.contains( value ) match {
            case true => Nil
            case false => List(Err("OPTION", "Invalid Option"))
        }
    }

}

