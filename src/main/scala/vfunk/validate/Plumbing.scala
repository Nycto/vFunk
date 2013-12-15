/**
 * Validators covering the plumbing
 */

package com.roundeights.vfunk.validate

import com.roundeights.vfunk.{Validator, Err}

/**
 * Returns the given errors
 */
class Manual (
    private val errors: Traversable[Err] = List()
) extends Validator {

    /** Alternate constructor for more fluently creating a validator */
    def this ( errors: Err* ) = this( errors )

    /** {@inheritDoc */
    override def getErrors ( value: String ) = errors.toList

    /** {@inheritDoc} */
    override def toString
        = "Validate(Errors(%s))".format( errors.mkString(", ") )
}

/**
 * Invokes a callback as a validator
 */
class Invoke (
    private val callback: (String) => Traversable[Err]
) extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = callback(value).toList

    /** {@inheritDoc} */
    override def toString = "Validate(Invoke(%s))".format( callback )
}

/**
 * Checks to see if a value is contained within a Set
 */
class In ( private val options: Set[String] ) extends Validator {

    /** Alternate constructor for creating a validator from a list of strings */
    def this ( options: String* ) = this( options.toSet )

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        options.contains( value ) match {
            case true => Nil
            case false => List(Err("OPTION", "Invalid Option"))
        }

    }

    /** {@inheritDoc} */
    override def toString = "Validate(In(%s))".format( options.mkString(", ") )
}

/**
 * Changes the error message returned by a given validator
 */
class ErrMessage (
    private val inner: Validator,
    private val error: String
) extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        inner.getErrors( value ) match {
            case Nil => Nil
            case List( Err(code, _) ) => List(Err(code, error))
            case List( Err(code, _), _@_* ) => List(Err(code, error))
        }
    }

    /** {@inheritDoc} */
    override def toString = inner.toString
}

