/**
 * Validators target specifically at strings
 */

package main.scala.vfunk.validate

import scala.util.matching.Regex

/**
 * A validator that matches strings that contain alpha-numeric characters
 */
class AlphaNum extends Validator {
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
class Equals (
    private val versus: String, private val caseSensitive: Boolean = false
) extends Validator {

    /**
     * The precomputed comparison value
     */
    private val compare = if (caseSensitive) versus else versus.toLowerCase

    override def getErrors ( value: String ) = {
        val against = if (caseSensitive) value else value.toLowerCase
        (against == compare) match {
            case true => Nil
            case false => List(
                Err("EQUALS", "Must be equal to '%s'".format(versus))
            )
        }
    }
}

/**
 * Validates that a value doesn't contain any spaces or new lines
 */
class NoWhitespace extends Validator {
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

    def this ( regex: String ) = this( regex.r )

    override def getErrors ( value: String ) = {
        regex.findFirstIn( value ) match {
            case Some(_) => Nil
            case None => List(
              Err("REGEX", "Must match the regular expression" + regex)
            )
        }
    }
}
