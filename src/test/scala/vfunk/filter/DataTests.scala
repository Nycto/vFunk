package test.roundeights.vfunk.filter

import org.specs2.mutable._

import com.roundeights.vfunk.filter._

class FilterDataTests extends Specification {

    "An EMail filter" should {
        val filter = new EMail
        "Remove invalid email characters from a string" in {
            val data = FilterHelper.build(0 to 255)
            filter.filter(data) must_== "!#$%&'*+-/0123456789" +
                "=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
                "^_`abcdefghijklmnopqrstuvwxyz{|}~"
        }
    }

    "A URL filter" should {
        val filter = new URL
        "Remove invalid URL characters from a string" in {
            val data = FilterHelper.build(0 to 255)
            filter.filter(data) must_== "!#$&'()*+,-./0123456789:;=?@" +
                "ABCDEFGHIJKLMNOPQRSTUVWXYZ[]" +
                "_abcdefghijklmnopqrstuvwxyz~"
        }
    }

    "An IPv4 filter" should {
        val filter = new IPv4
        "Remove invalid IPv4 characters from a string" in {
            val data = FilterHelper.build(0 to 255)
            filter.filter(data) must_== ".0123456789"
        }
    }

    "An IPv6 filter" should {
        val filter = new IPv6
        "Remove invalid IPv6 characters from a string" in {
            val data = FilterHelper.build(0 to 255)
            filter.filter(data) must_== ".0123456789:ABCDEFabcdef"
        }
    }

}

