/**
 * Plumbing level filters
 */

package main.scala.vfunk.filter

/**
 * A filter that trims the white space from the given string
 */
class Trim extends Filter {
    override def filter ( value: String ) = value.trim
}

