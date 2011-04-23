package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.validate._

@RunWith(classOf[JUnitSuiteRunner])
class ValidationNumericTests extends Specification with JUnit {
    "An IsNumeric validator" should {
        val validator = new IsNumeric
        "Properly detect numbers" in {
            validator must validateFor(
                "0", "1", "-1", "-0", "+0", "0.0", "-0.0", "+0.0", "1.0",
                "-1.0", "+1.0", ".5", "-.5", "+.5", "-.5e-2", ".5e-2",
                "+.5e-2", "+.5E+2", "0.70000000", "+0.70000000", "-0.70000000",
                "1234567890123456", "-1234567890123456",
                "984847472827282718178", "-984847472827282718178", "123.56e30",
                "123.56E30", "426.45e-30", "5657.3E-40", "3486.36e+40",
                "3486.36E+90", "-3486.36E+10", "-3486.36e+80", "-426.45e-50",
                "-426.45E-99", "1e2", "-1e2", "-1e-2", "+1e2", "+1e+2", "+1e-2",
                "+1e+2", "2245555555555555.444", "1.444444444444444444",
                "01000000000000000000000", "0123", "0345900", "-0200001",
                "-0200001.7", "0200001.7", "+0200001", "+0200001.7",
                "+0200001.7", "2.00000000000000000000001", "1", "-1", "1e2",
                " 1", "2974394749328742328432", "-1e-2", "1", "-1", "1e2",
                " 1", "2974394749328742328432", "-1e-2", "0123", "0123",
                "-0123", "+0123", "-0123", "+0123", "1 "
            )
        }
        "Reject invalid values" in {
            validator must notValidateFor(
                "0xff", "0xFF", "-0x1111111", "+0x6698319",
                "-0x80001", "+0x80001", "-0x80001.5",
                "0x80001.5", "", "- 1", "1.2.4", "1e7.6",
                "3FF", "20 test", "3.6test", "1,000",
                "NULL", "true"
            )
        }
    }
    "An Odd validator" should {
        val validator = new Odd
        "Properly validate numbers" in {
            validator must validateFor( "-5", "9", "3", "71" )
            validator must notValidateFor( "0", "4", "-20", "100" )
        }
        "Fail for non-numbers" in {
            validator must notValidateFor( "abc" )
        }
    }
}
