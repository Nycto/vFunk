/**
 * Encoding specific data filters
 */

package com.roundeights.vfunk.filter

import com.roundeights.vfunk.Filter

/**
 * Filters out any non Hex valid characters
 */
class Hex extends Characters (
    Set[Char]() ++ ('a' to 'f') ++ ('A' to 'F') ++ ('0' to '9')
) {

    /** {@inheritDoc} */
    override def toString = "Filter(Hex)"
}


