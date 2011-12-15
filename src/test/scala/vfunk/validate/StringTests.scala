package test.scala.vfunk.validate

import org.specs2.mutable._

import main.scala.vfunk.validate._

class ValidationStringTests extends Specification {

    "An AlphaNum validator" should {
        val validator = new AlphaNum

        "pass when the string is AlphaNumeric" in {
            validator must validateFor(
                "string", "test123", "abc123XYZ",
                "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                "abcdefghijklmnopqrstuvwxyz",
                "0123456789"
            );
        }
        "fail when the string contains other characters" in {
            validator must notValidateFor(
                "!\"#$%&'()*+,-/:;<=>?@[\\]^`{|}~",
                "Has Some Spaces"
            );
        }
    }

    "An Alpha validator" should {
        val validator = new Alpha

        "pass when the string is alphabetical" in {
            validator must validateFor(
                "string", "abcXYZ", "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                "abcdefghijklmnopqrstuvwxyz"
            );
        }
        "fail when the string contains non-alphabetic characters" in {
            validator must notValidateFor(
                "test123", "!\"#$%&'()*+,-/:;<=>?@[\\]^`{|}~",
                "Has Some Spaces", "0123456789"
            );
        }
    }

    "A Digit validator" should {
        val validator = new Digit

        "pass when the string is digits" in {
            validator must validateFor("0123456789")
        }
        "fail when the string contains non-digit characters" in {
            validator must notValidateFor(
                "test123", "!\"#$%&'()*+,-/:;<=>?@[\\]^`{|}~",
                "Has Some Spaces", "abcdefghijklmnopqrstuvwxyz"
            );
        }
    }

    "A Same Validator" should {
        val caseInsensitive = new Same("Some String")
        val caseSensitive = new Same("Some String", true)

        "Pass when strings are exactly equal" in {
            caseInsensitive must validateFor("Some String")
        }
        "Pass for the default case-sensitive comparison" in {
            caseInsensitive must validateFor("some string")
        }
        "Fail when the strings are different" in {
            caseInsensitive must notValidateFor("another string")
        }
        "Pass when strings are exactly equal for case-sensitive checks" in {
            caseSensitive must validateFor("Some String")
        }
        "Fail when case-sensitivity is flagged" in {
            caseSensitive must notValidateFor("some string")
        }
    }

    "A Whitespace validator" should {
        val validator = new NoWhitespace

        "pass for strings without whitespace" in {
            validator must validateFor(
                "string", "test123", "abc123XYZ"
            )
        }
        "fail for strings with whitespace" in {
            validator must notValidateFor(
                "Has\nSome\nNew\nLines",
                "Has\rCarriage\rReturns",
                "Has Some Spaces"
            )
        }
    }

    "A RegExp validator" should {
        val validator = new RegExp("simple");

        "Pass when the expression matches" in {
            validator must validateFor("simple")
        }
        "Fail when the expression doesn't match" in {
            validator must notValidateFor("oops")
        }
    }

    "A NotBlank validator" should {
        val validator = new NotBlank

        "Pass when the string isn't blank" in {
            validator must validateFor(
                " This is a string ", "Another\nOne"
            )
        }
        "Fail when the expression doesn't match" in {
            validator must notValidateFor(
                "", "   ", "\n\r\t"
            )
        }
    }

}

