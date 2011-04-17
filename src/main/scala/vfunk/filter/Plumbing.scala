/**
 * Plumbing level filters
 */

package main.scala.vfunk.filter

/**
 * A filter that simply returns the value it is given
 */
class Identity extends Filter {
    override def filter ( value: String ) = value
}

