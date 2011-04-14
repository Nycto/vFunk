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
            "^", byte, """(?:\.""" + byte + """){3}""", "$"
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

/**
 * Validates an IPv6 address
 */
class IPv6 extends Validator {
    lazy private val regexp = {
        val hex = "[0-9a-fA-F]{1,4}"
        val hexc = "(?:" + hex + ":)"
        val chex = "(?::" + hex + ")"

        val byte = """(?:(?:25[0-5])|(?:2[0-4][0-9])|(?:1[0-9]{2})|(?:[1-9]?[0-9]))"""
        val ipv4 = "(?:" + byte + """(?:\.""" + byte + "){3})"

        val regex = List(
            "^",
            "(?:",
                "(", hexc, "{7}(", hex, "|:))",
                "|(", hexc, "{6}(:", hex, "|", ipv4, "|:))",
                "|(", hexc, "{5}((", chex, "{1,2})|:", ipv4, "|:))",
                "|(", hexc, "{4}((", chex, "{1,3})|(", chex, "?:", ipv4, ")|:))",
                "|(", hexc, "{3}((", chex, "{1,4})|(", chex, "{0,2}:", ipv4, ")|:))",
                "|(", hexc, "{2}((", chex, "{1,5})|(", chex, "{0,3}:", ipv4, ")|:))",
                "|((", hexc, "){1}((", chex, "{1,6})|(", chex, "{0,4}:", ipv4, ")|:))",
                "|(:((", chex, "{1,7})|(", chex, "{0,5}:", ipv4, ")|:))",
            ")",
            "$"
        )
        regex.reduceLeft(_ + _).r
    }

    override def getErrors ( value: String ) = {
        regexp.findFirstIn( value ) match {
            case None => List( Err("IPV6", "Invalid IP address") )
            case Some(_) => Nil
        }
    }
}

