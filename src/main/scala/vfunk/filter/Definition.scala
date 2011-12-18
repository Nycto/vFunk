/**
 * Structural code for defining the Filter interface
 */

package com.roundeights.vfunk

/**
 * A builder for generating filters
 */
object Filter {

    import com.roundeights.vfunk.filter._

    def email = new EMail
    def url = new URL
    def ipv4 = new IPv4
    def ipv6 = new IPv6
    def identity = new Identity
    def callback( callback: (String) => String ) = new Callback( callback )
    def chain ( filters: Traversable[Filter] ) = new Chain( filters )
    def chain ( filters: Filter* ) = new Chain( filters:_* )
    def trim = new Trim
    def alphaNum = new AlphaNum
    def alpha = new Alpha
    def digit = new Digit
    def hex = new Hex
    def printable = new Printable
    def numeric = new Numeric
    def characters ( chars: Set[Char] ) = new Characters( chars )
    def characters ( chars: String ) = new Characters( chars )
    def characters ( chars: Char* ) = new Characters( chars:_* )

}

/**
 * Applies a transformation to a string, resulting in another string
 */
trait Filter {

    /**
     * Applies a transformation and returns the result
     */
    def filter ( value: String ): String

}

