package com.roundeights.vfunk

/**
 * Helper methods for creating validators
 */
object Validate {

    import com.roundeights.vfunk.validate._
    import scala.util.matching.Regex

    def email = new Email
    def ipv4 = new IPv4
    def ipv6 = new IPv6
    def minLength ( length: Int ) = new MinLength( length )
    def maxLength ( length: Int ) = new MaxLength( length )
    def exactLength ( length: Int ) = new ExactLength( length )
    def length ( length: Int ) = exactLength( length )
    def notEmpty = new NotEmpty

    def and( validators: List[Validator] ) = new And( validators )
    def and( validators: Validator* ) = new And( validators:_* )

    def or( validators: List[Validator] ) = new Or( validators )
    def or( validators: Validator* ) = new Or( validators:_* )

    def not (
        validator: Validator,
        message: String = "Value failed validation"
    ) = new Not( validator, message )

    def isNumeric = new IsNumeric
    def odd = new Odd
    def even = new Even

    def equals ( vs: Number ) = new Equals( vs )
    def == ( vs: Number ) = new Equals( vs )

    def lessThan ( vs: Number ) = new LessThan( vs )
    def < ( vs: Number ) = new LessThan( vs )

    def lessThanEquals ( vs: Number ) = new LessThanEquals( vs )
    def <= ( vs: Number ) = new LessThanEquals( vs )

    def greaterThan ( vs: Number ) = new GreaterThan( vs )
    def > ( vs: Number ) = new GreaterThan( vs )

    def greaterThanEquals ( vs: Number ) = new GreaterThanEquals( vs )
    def >= ( vs: Number ) = new GreaterThanEquals( vs )

    def manual( errors: List[Err] ) = new Manual( errors )
    def manual( errors: Err* ) = new Manual( errors )

    def invoke (callback: (String) => Traversable[Err]) = new Invoke(callback)

    def in ( options: Set[String] ) = new In( options )
    def in ( options: String* ) = new In( options:_* )

    def alphaNum = new AlphaNum
    def alpha = new Alpha
    def digit = new Digit
    def hex = new Hex

    def chars( allowed: Set[Char] ) = new Characters( allowed )
    def chars( allowed: Seq[Char]* ) = new Characters( allowed:_* )
    def chars( allowed: String ) = new Characters( allowed )

    def same ( versus: String, caseSensitive: Boolean = false )
        = new Same(versus, caseSensitive)

    def noWhitespace = new NoWhitespace
    def regExp ( regex: Regex ) = new RegExp( regex )
    def regExp ( regex: String ) = new RegExp( regex )
    def notBlank = new NotBlank

    def contains( chars: Set[Char] ) = new Contains( chars )
    def contains( chars: Seq[Char]* ) = new Contains( chars:_* )
    def contains( chars: String ) = new Contains( chars )
}


