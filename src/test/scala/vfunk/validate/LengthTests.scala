package test.roundeights.vfunk.validate

import org.specs2.mutable._

import com.roundeights.vfunk.validate._

class ValidationLengthTests extends Specification {

    "A MinLength Validator" should {
        val validator = new MinLength(5)
        "pass when a string is long enough" in {
            validator must validateFor("long enough")
        }
        "fail when a string is too short" in {
            validator must notValidateFor("data");
        }
    }

    "A MaxLength Validator" should {
        val validator = new MaxLength(5)
        "pass when a string is short enough" in {
            validator must validateFor("data")
        }
        "fail when a string is too long" in {
            validator must notValidateFor("too long")
        }
    }

    "An ExactLength Validator" should {
        val validator = new ExactLength(4)
        "pass when a string is the right length" in {
            validator must validateFor("data")
        }
        "fail when a string not the right length" in {
            validator must notValidateFor("some data")
        }
    }

    "A NotEmpty Validator" should {
        val validator = new NotEmpty
        "pass when a string is not empty" in {
            validator must validateFor("data")
        }
        "fail when a string is empty" in {
            validator must notValidateFor("")
        }
    }

}

