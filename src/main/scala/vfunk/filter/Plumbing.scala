/**
 * Plumbing level filters
 */

package com.roundeights.vfunk.filter

import com.roundeights.vfunk.Filter

/**
 * A filter that simply returns the value it is given
 */
class Identity extends Filter {

    /** {@inheritDoc */
    override def filter ( value: String ) = value

}

/**
 * A filter that allows you to wrap a callback
 */
class Callback ( private val callback: (String) => String ) extends Filter {

    /** {@inheritDoc */
    override def filter ( value: String ) = callback( value )

}

/**
 * A filter that string together a list of other filters and
 * pipes the result of one into the next
 */
class Chain ( private val chain: Traversable[Filter] ) extends Filter {

    /**
     * Alternate constructor for more fluently creating a filter
     */
    def this ( filters: Filter* ) = this( filters )

    /** {@inheritDoc */
    override def filter ( value: String ) = {
        chain.foldLeft (value) { (accum, filter) => filter.filter(accum) }
    }

}
