package test.scala.vfunk.validate

import org.specs2.mutable._

import main.scala.vfunk.validate._

class ValidationNumericComparisonTests extends Specification {

    val intEquals = List("5");
    val intLess = List("4.99999", "4", "-100");
    val intGreater = List("5.000001", "6", "10000000000");

    val floatEquals = List("3.1415")
    val floatLess = List("3.1414", "-100", "3")
    val floatGreater = List("3.1416", "1000000000", "4")

    val err = List("abc123")

    "An Equals validator" should {
        "Properly compare integers" in {
            val validator = new Equals(5)
            validator must validateFor( intEquals )
            validator must notValidateFor( intLess ++ intGreater ++ err )
        }
        "Properly compare floats" in {
            val validator = new Equals(3.1415)
            validator must validateFor( floatEquals )
            validator must notValidateFor( floatLess ++ floatGreater ++ err )
        }
    }

    "A LessThan validator" should {
        "Properly compare integers" in {
            val validator = new LessThan(5)
            validator must validateFor( intLess )
            validator must notValidateFor( intEquals ++ intGreater ++ err )
        }
        "Properly compare floats" in {
            val validator = new LessThan(3.1415)
            validator must validateFor( floatLess )
            validator must notValidateFor( floatEquals ++ floatGreater++ err )
        }
    }

    "A LessThanEquals validator" should {
        "Properly compare integers" in {
            val validator = new LessThanEquals(5)
            validator must validateFor( intLess ++ intEquals )
            validator must notValidateFor( intGreater ++ err )
        }
        "Properly compare floats" in {
            val validator = new LessThanEquals(3.1415)
            validator must validateFor( floatLess ++ floatEquals )
            validator must notValidateFor( floatGreater++ err )
        }
    }

    "A GreaterThan validator" should {
        "Properly compare integers" in {
            val validator = new GreaterThan(5)
            validator must validateFor( intGreater )
            validator must notValidateFor( intLess ++ intEquals ++ err )
        }
        "Properly compare floats" in {
            val validator = new GreaterThan(3.1415)
            validator must validateFor( floatGreater )
            validator must notValidateFor( floatLess ++ floatEquals ++ err )
        }
    }

    "A GreaterThanEquals validator" should {
        "Properly compare integers" in {
            val validator = new GreaterThanEquals(5)
            validator must validateFor( intGreater ++ intEquals )
            validator must notValidateFor( intLess ++ err )
        }
        "Properly compare floats" in {
            val validator = new GreaterThanEquals(3.1415)
            validator must validateFor( floatGreater ++ floatEquals )
            validator must notValidateFor( floatLess ++ err )
        }
    }

}

