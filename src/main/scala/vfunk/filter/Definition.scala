/**
 * Structural code for defining the Filter interface
 */

package com.roundeights.vfunk.filter

/**
 * Applies a transformation to a string, resulting in another string
 */
trait Filter {

    /**
     * Applies a transformation and returns the result
     */
    def filter ( value: String ): String

}

