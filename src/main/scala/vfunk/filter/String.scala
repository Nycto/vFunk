/**
 * Plumbing level filters
 */

package com.roundeights.vfunk.filter

import com.roundeights.vfunk.Filter

import scala.collection.immutable.NumericRange

/**
 * A helper class for testing character values
 */
private object StringFilters {

    /**
     * Returns whether a character falls within a given character code range
     */
    private def in( chr: Char, low: Int, high: Int ) = {
        chr >= low && chr <= high
    }

    /**
     * Returns whether a character is a strict alpha character
     *
     * This is more strict than Character.isLetter
     */
    def isLetter( chr: Char ) = in(chr, 65, 90) || in(chr, 97, 122)

    /**
     * Returns whether a character is a strict alpha-numeric character
     */
    def isLetterOrDigit( chr: Char ) = isLetter(chr) || Character.isDigit(chr)

    /**
     * Whether a character is considered printable
     */
    def isPrintable( chr: Char ) = in(chr, 32, 126)

}

/**
 * A filter that trims the white space from the given string
 */
class Trim extends Filter {

    /** {@inheritDoc */
    override def filter ( value: String ) = value.trim

}

/**
 * Removes any non-alphanumeric characters from a string
 */
class AlphaNum extends Filter {

    /** {@inheritDoc */
    override def filter ( value: String )
        = value.filter { StringFilters.isLetterOrDigit(_)  }

}

/**
 * Removes any non-alphabetic characters from a string
 */
class Alpha extends Filter {

    /** {@inheritDoc */
    override def filter ( value: String )
        = value.filter { StringFilters.isLetter(_)  }

}

/**
 * Removes any non-digit characters from a string
 */
class Digit extends Filter {

    /** {@inheritDoc */
    override def filter ( value: String )
        = value.filter { Character.isDigit(_)  }

}

/**
 * Removes any non-printable characters from a string
 */
class Printable extends Filter {

    /** {@inheritDoc */
    override def filter ( value: String )
        = value.filter { StringFilters.isPrintable(_)  }

}

/**
 * Removes any character not in the given list
 */
class Characters ( private val valid: Set[Char] ) extends Filter {

    /**
     * Creates a character filter from a string
     */
    def this ( valid: String ) = this( Set( valid.toList:_* ) )

    /**
     * Creates a character filter from a list of characters
     */
    def this ( valid: Char* ) = this( Set( valid:_* ) )

    /**
     * Creates a character filter from a range of characters
     */
    def this ( valid: NumericRange[Char] ) = this( Set( valid:_* ) )

    /** {@inheritDoc */
    override def filter ( value: String ) = value.filter { valid.contains(_)  }

}

