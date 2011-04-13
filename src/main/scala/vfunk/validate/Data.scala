/**
 * Validators for specific types of Data
 */

package main.scala.vfunk.validate

/**
 * Validates an email address
 */
class Email extends Validator {

    /**
     * Taken from http://www.regular-expressions.info/email.html
     *
     * This doesn't handle IP email addresses, but that is such an edge
     * case that I don't think it's really worth it.
     */
    lazy private val regexp = {
        val regex = List(
            "(?i)",
            "^",
            """[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*""",
            "@",
            """(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?""",
            "$"
        )
        regex.reduceLeft(_ + _).r
    }

    override def getErrors ( value: String ) = {
        regexp.findFirstIn( value ) match {
            case None => List( Err("EMAIL", "Invalid e-mail address") )
            case Some(_) => Nil
        }
    }
}

/**
 * Validates an IPv4 address
 */
class IPv4 extends Validator {
    lazy private val regexp = {
        val byte = """(?:(?:25[0-5])|(?:2[0-4][0-9])|(?:1[0-9]{2})|(?:[1-9]?[0-9]))"""
        val regex = List(
            "^",
            byte,
            """(?:\.""" + byte + """){3}""",
            "$"
        )
        regex.reduceLeft(_ + _).r
    }

    override def getErrors ( value: String ) = {
        regexp.findFirstIn( value ) match {
            case None => List( Err("IPV4", "Invalid IP address") )
            case Some(_) => Nil
        }
    }
}
