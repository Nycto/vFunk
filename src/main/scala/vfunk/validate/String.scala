/**
 * Validators target specifically at strings
 */

package com.roundeights.vfunk.validate

import com.roundeights.vfunk.{Validator, Err}

import scala.util.matching.Regex

/**
 * A validator that matches strings that contain alpha-numeric characters
 */
class AlphaNum extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        value.forall { Character.isLetterOrDigit(_) } match {
            case true => Nil
            case false => List(
                Err("ALPHANUM", "Must only contain letters and numbers")
            )
        }
    }

}

/**
 * A validator that matches strings that contain alphabetic characters
 */
class Alpha extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        value.forall { Character.isLetter(_) } match {
            case true => Nil
            case false => List(Err("ALPHA", "Must only contain letters"))
        }
    }

}

/**
 * A validator that matches strings that contain numeric characters
 */
class Digit extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        value.forall { Character.isDigit(_) } match {
            case true => Nil
            case false => List(Err("DIGIT", "Must only contain numbers"))
        }
    }

}

/**
 * Validates that the result equals the given value
 */
class Same (
    private val versus: String, private val caseSensitive: Boolean = false
) extends Validator {

    /**
     * The precomputed comparison value
     */
    private val compare = if (caseSensitive) versus else versus.toLowerCase

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        val against = if (caseSensitive) value else value.toLowerCase
        (against == compare) match {
            case true => Nil
            case false => List(
                Err("SAME", "Must be equal to '%s'".format(versus))
            )
        }
    }

}

/**
 * Validates that a value doesn't contain any spaces or new lines
 */
class NoWhitespace extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        value.exists { Character.isWhitespace(_) } match {
            case false => Nil
            case true => List(Err("WHITESPACE", "Must not contain spaces"))
        }
    }

}

/**
 * Validates that the value matches a regular expression
 */
class RegExp ( private val regex: Regex ) extends Validator {

    /**
     * Alternate constructor that builds a regex validator from a string
     */
    def this ( regex: String ) = this( regex.r )

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        regex.findFirstIn( value ) match {
            case Some(_) => Nil
            case None => List(
                Err("REGEX", "Must match the regular expression: " + regex)
            )
        }
    }

}

/**
 * Validates that a value contains more than just white
 */
class NotBlank extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        value.trim == "" match {
            case false => Nil
            case true => List( Err("NOTBLANK", "Must not be blank") )
        }
    }

}

/**
 * Validates that a string only contains hex characters
 */
class Hex extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        value.forall { "0123456789abcdefABCDEF".indexOf(_) >= 0 } match {
            case true => Nil
            case false => List( Err("HEX", "Must be a hex string") )
        }
    }
}

/**
 * Validates that a string only contains the given characters
 */
class Characters ( private val valid: Set[Char] ) extends Validator {

    /** Generates a characters validator from a strings */
    def this ( valid: String ) = this( valid.toSet )

    /** Generates a characters validator from a range of characters */
    def this ( valid: Seq[Char]* )
        = this( valid.foldLeft( Set[Char]() )( _ ++ _.toSet ) )

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        value.forall { valid.contains(_) } match {
            case true => Nil
            case false => List( Err("CHARS", "Contains invalid characters") )
        }
    }
}

