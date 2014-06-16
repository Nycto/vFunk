package com.roundeights.vfunk.validate

import com.roundeights.vfunk.{Validator, Validated, Err}

import scala.util.matching.Regex

/**
 * A validator that matches strings that contain alpha-numeric characters
 */
class AlphaNum extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        Validated(
            value.forall( Character.isLetterOrDigit(_) ),
            Err("ALPHANUM", "Must only contain letters and numbers")
        )
    }

    /** {@inheritDoc} */
    override def toString = "Validate(AlphaNum)"
}

/**
 * A validator that matches strings that contain alphabetic characters
 */
class Alpha extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        Validated(
            value.forall( Character.isLetter(_) ),
            Err("ALPHA", "Must only contain letters")
        )
    }

    /** {@inheritDoc} */
    override def toString = "Validate(Alpha)"
}

/**
 * A validator that matches strings that contain numeric characters
 */
class Digit extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        Validated(
            value.forall( Character.isDigit(_) ),
            Err("DIGIT", "Must only contain numbers")
        )
    }

    /** {@inheritDoc} */
    override def toString = "Validate(Digit)"
}

/**
 * Validates that the result equals the given value
 */
class Same (
    private val versus: String, private val caseSensitive: Boolean = false
) extends Validator {

    /** The precomputed comparison value */
    private val compare = if (caseSensitive) versus else versus.toLowerCase

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        val against = if (caseSensitive) value else value.toLowerCase
        Validated(
            against == compare,
            Err("SAME", "Must be equal to '%s'".format(versus))
        )
    }

    /** {@inheritDoc} */
    override def toString = "Validate(== '%s')".format(versus)
}

/**
 * Validates that a value doesn't contain any spaces or new lines
 */
class NoWhitespace extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        Validated(
            !value.exists(Character.isWhitespace(_)),
            Err("WHITESPACE", "Must not contain spaces")
        )
    }

    /** {@inheritDoc} */
    override def toString = "Validate(NoWhitespace)"
}

/**
 * Validates that the value matches a regular expression
 */
class RegExp ( private val regex: Regex ) extends Validator {

    /** Alternate constructor that builds a regex validator from a string */
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

    /** {@inheritDoc} */
    override def toString = "Validate(RegExp(%s))".format( regex )
}

/**
 * Validates that a value contains more than just white
 */
class NotBlank extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String )
        = Validated( value.trim != "", Err("NOTBLANK", "Must not be blank") )

    /** {@inheritDoc} */
    override def toString = "Validate(NotBlank)"
}

/**
 * Validates that a string only contains hex characters
 */
class Hex extends Validator {

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        Validated(
            value.forall("0123456789abcdefABCDEF".indexOf(_) >= 0),
            Err("HEX", "Must be a hex string")
        )
    }

    /** {@inheritDoc} */
    override def toString = "Validate(Hex)"
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
        Validated(
            value.forall(valid.contains(_)),
            Err("CHARS", "Contains invalid characters")
        )
    }

    /** {@inheritDoc} */
    override def toString = "Validate(Characeters(%s))".format( valid.mkString )
}

/**
 * Validates that the string contains at least one of the given characters
 */
class Contains ( private val valid: Set[Char] ) extends Validator {

    /** Generates a characters validator from a strings */
    def this ( valid: String ) = this( valid.toSet )

    /** Generates a characters validator from a range of characters */
    def this ( valid: Seq[Char]* )
        = this( valid.foldLeft( Set[Char]() )( _ ++ _.toSet ) )

    /** {@inheritDoc */
    override def getErrors ( value: String ) = {
        Validated(
            value.exists(valid.contains(_)),
            Err("CONTAINS", "Missing required characters")
        )
    }

    /** {@inheritDoc} */
    override def toString = "Validate(Contains(%s))".format( valid.mkString )
}

