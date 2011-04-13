package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.validate._

@RunWith(classOf[JUnitSuiteRunner])
class DataTests extends Specification with JUnit {

    "An Email Validator" should {
        val validator = new Email
        "pass for valid email addresses" in {
            validator must validateFor(
                "abc@example.com", "Abc@example.com",
                "aBC@example.com", "abc.123@example.com",
                "abc.123@sub.example.com", "abc.123@sub.sub.example.com",
                "abc+123@example.com", "1234567890@example.com",
                "_______@example.com", "test?mail@example.com",
                "abc+mailbox/department=shipping@example.com",
                "test=mail@example.com", "test-mail@example.com",
                "customer/department=shipping@example.com",
                "$A12345@example.com", "!def!xyz%abc@example.com",
                "_somename@example.com"
            )
        }
        "fail for invalid email addresses" in {
            validator must notValidateFor(
                "", "   ", "@", "Abc.example.com",
                "A@b@c@example.com", "Abc..123@example.com",
                "Abc. 123@ example.com", "Abc.\n123@example.com",
                "Abc.\r123@example.com", "Abc123@\texample.com",
                "()[];:,<>@example.com", "Abc@example.com.",
                ".Abc@example.com", "Abc.@example.com",
                "Abc@.example.com", "@example.com", "abc@",
                "abc@example", "foo@-foo.com", "foo@foo-.com"
            )
        }
    }
    "An IPv4 Validator" should {
        val validator = new IPv4
        "pass for valid IP addresses" in {
            validator must validateFor(
                "0.0.0.0", "127.0.0.1", "1.1.1.1",
                "255.255.255.255"
            )
        }
        "fail for invalid IP addresses" in {
            validator must notValidateFor(
                "", "   ", "127.127.127.",
                "  1.1.1.1   ", "1.1.1", "1.1.1.1."
            )
        }
    }

}
