package test.roundeights.vfunk.filter

import org.specs2.mutable._

import scala.collection.immutable.HashSet

import com.roundeights.vfunk.filter._

class EncodingTests extends Specification {

    "A Hex filter" should {
        val filter = new Hex
        "Leave a string with only hex characters unchanged" in {
            val data = "0123456789abcdefABCDEF"
            filter.filter(data) must_== data
        }
        "Strip hex characters from a string" in {
            val data =  FilterHelper.build(
                0 to 47, 58 to 64, 71 to 96, 103 to 255
            )
            filter.filter(data) must_== ""
        }
    }

}

