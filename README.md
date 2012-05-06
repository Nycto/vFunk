vFunk
=====

vFunk is a simple data filtering and validation library for Scala. It is
designed to make user input processing simple and composable. It is primarily
targeted at processing Strings.

Filters
-------

Filters take one string for input and return a slightly modified string. For
example, you might want to trim the whitespace from the edges of a string. Or
you might want to remove non-alphanumeric characters. That sort of thing.

The available filters are:

* _Email_: Filters out invalid email address characters
* _URL_: Filters out invalid URL characters
* _IPv4_: Filters out invalid IPv4 characters
* _IPv6_: Filters out invalid IPv6 characters
* _Hex_: Filters out any non Hex valid characters
* _Identity_: Returns the value it is given
* _Callback_: Wrap a custom callback
* _Chain_: Allows multiple filters to be chained together. The result of each
  filter are automatically passed into the next filter
* _Trim_: Trims the whitespace from the given string
* _AlphaNum_: Removes any non-alphanumeric characters from a string
* _Alpha_: Removes any non-alphabetic characters from a string
* _Digit_: Removes any non-digit characters from a string
* _Printable_: Removes any non-printable characters from a string
* _Replace_: Performs a literal search and replace on the incoming string
* _Characters_: Removes any character not in a given list
* _Numeric_: Filters out any non-numeric characters. Meaning 0-9, dash (-)
  and period (.)

Validators
----------

Validators examine a string and determine whether it meets a predetermined set
of requirements.

Validators will return a `Validated` object, which represents the success of
the validation pass. It contains a list of any errors that occurred and the
value that was validated.

* _Email_: Validates an email address
* _IPv4_: Validates an IPv4 address
* _IPv6_: Validates an IPv6 address
* _MinLength_: Validates that the string is at least a given length
* _MaxLength_: Validates that the string is no longer than a given length
* _ExactLength_: Validates that the string is exactly a given length
* _NotEmpty_: Validates that a string isn't empty
* _And_: Validates that a list of sub-validators all validate
* _Or_: Requires any of its sub-validators to pass
* _Not_: Inverts the results of a contained validator
* _IsNumeric_: Validates that a value is a number
* _Odd_: Validates that a value is an odd number
* _Even_: Validates that a value is an even number
* _Equals_: Validates whether two values are numerically equal
* _LessThan_: Validates that a value is less than a given value
* _LessThanEquals_: Validates that a value is less than or equal to a given
  value
* _GreaterThan_: Validates that a value is greater than a given value
* _GreaterThanEquals_: Validates that a value is greater than or equal to a
  given value
* _Manual_: Always returns a given list of errors
* _Invoke_: Invokes a callback as a validator
* _In_: Checks to see if a value is contained within a Set
* _AlphaNum_: Matches strings that contain alpha-numeric characters
* _Alpha_: Matches strings that contain alphabetic characters
* _Digit_: Matches strings that contain numeric characters
* _Same_: Validates that the result equals the given value
* _NoWhitespace_: Validates that a value doesn't contain any spaces or new lines
* _RegExp_: Validates that the value matches a regular expression
* _NotBlank_: Validates that a value contains more than just white

Fields
------

Fields are a Filter and a Validator joined together and given a name.

Forms
-----

Forms are a list of fields joined together. They are useful for processing an
entire map of values at once.

