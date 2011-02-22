/**
 * Validators target specifically at strings
 */

package main.scala.vfunk.validate

/**
 * A validator that matches strings that contain alpha-numeric characters
 */
class AlphaNum extends Validator {
    override def getErrors ( value: String ) = {
        value.forall { Character.isLetterOrDigit(_) } match {
            case true => Nil
            case false => List(
                Err("ALPHANUM", "Must only contain letters and numbers")
            )
        }
    }
}

/**
 * A validator that matches strings that contain alphabetic characters
 */
class Alpha extends Validator {
    override def getErrors ( value: String ) = {
        value.forall { Character.isLetter(_) } match {
            case true => Nil
            case false => List(Err("ALPHA", "Must only contain letters"))
        }
    }
}

/**
 * A validator that matches strings that contain numeric characters
 */
class Digit extends Validator {
    override def getErrors ( value: String ) = {
        value.forall { Character.isDigit(_) } match {
            case true => Nil
            case false => List(Err("DIGIT", "Must only contain numbers"))
        }
    }
}

