/**
 * Plumbing level filters
 */

package main.scala.vfunk.filter

import scala.collection.immutable.HashSet
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
    override def filter ( value: String ) = value.trim
}

/**
 * Removes any non-alphanumeric characters from a string
 */
class AlphaNum extends Filter {
    override def filter ( value: String ) = {
        value.filter { StringFilters.isLetterOrDigit(_)  }
    }
}

/**
 * Removes any non-alphabetic characters from a string
 */
class Alpha extends Filter {
    override def filter ( value: String ) = {
        value.filter { StringFilters.isLetter(_)  }
    }
}

/**
 * Removes any non-digit characters from a string
 */
class Digit extends Filter {
    override def filter ( value: String ) = {
        value.filter { Character.isDigit(_)  }
    }
}

/**
 * Removes any non-printable characters from a string
 */
class Printable extends Filter {
    override def filter ( value: String ) = {
        value.filter { StringFilters.isPrintable(_)  }
    }
}

/**
 * Removes any character not in the given list
 */
class Characters ( private val valid: Set[Char] ) extends Filter {

    def this ( valid: String ) = this( HashSet( valid.toList:_* ) )
    def this ( valid: Char* ) = this( HashSet( valid:_* ) )
    def this ( valid: NumericRange[Char] ) = this( HashSet( valid:_* ) )

    override def filter ( value: String ) = {
        value.filter { valid.contains(_)  }
    }
}

