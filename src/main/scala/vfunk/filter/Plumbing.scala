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

/**
 * A filter that allows you to wrap a callback
 */
class Callback ( private val callback: (String) => String ) extends Filter {
   override def filter ( value: String ) = callback( value )
}

/**
 * A filter that string together a list of other filters and
 * pipes the result of one into the next
 */
class Chain ( private val chain: Filter* ) extends Filter {
    override def filter ( value: String ) = {
        chain.foldLeft (value) { (accum, filter) => filter.filter(accum) }
    }
}
