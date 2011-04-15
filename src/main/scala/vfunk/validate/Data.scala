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
    private lazy val uncompressed = """(?i)^(?:[a-f0-9]{1,4}:){7}[a-f0-9]{1,4}$""".r
    private lazy val compressed = """(?i)^(?::|(?:[a-f0-9]{1,4}:)+):(?:(?:[a-f0-9]{1,4}:)*[a-f0-9]{1,4})?$""".r
    private lazy val IPv4 = new IPv4
    private lazy val err = Err("IPV6", "Invalid IP Address")

    override def getErrors ( value: String ) = {

        // For localhost
        if ( value.length <= 2 ) {
            value == "::" match {
                case false => List(err)
                case true => Nil
            }
        }

        // Check for IPv4 compatibility
        else if ( value.contains(".") ) {
            val lastcolon = value.lastIndexWhere( _ == ':' )

            if ( lastcolon == -1 ) {
                List(err)
            }
            else {
                val (left, right) = value.splitAt(lastcolon)
                if ( left.contains(".") )
                    List(err)
                else if ( !IPv4.isValid( right.drop(1) ) )
                    List(err)
                else
                    getErrors( left + ":0:0" )
            }
        }

        // check uncompressed
        else if ( !value.contains("::") ) {
            uncompressed.findFirstIn( value ) match {
                case None => List(err)
                case Some(_) => Nil
            }
        }

        // check colon-count for compressed format
        else if ( value.count( _ == ':' ) < 8 ) {
            compressed.findFirstIn( value ) match {
                case None => List(err)
                case Some(_) => Nil
            }
        }

        else {
            List(err)
        }
    }
}

