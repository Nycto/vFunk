package test.roundeights.vfunk

import org.specs2.mutable._

import com.roundeights.vfunk._
import com.roundeights.vfunk.validate._
import com.roundeights.vfunk.filter._

class FormTest extends Specification  {

    "A Form" should {

        val form = Form(
            TextField( "identity" ),
            TextField(
                "live",
                new Callback( _ match {
                    case "correct" => "filtered"
                    case _ => "fail"
                } ),
                new In( "filtered" )
            )
        )

        "validate a map of valid values" in {
            val result = form.process(
                "identity" -> "unchanged",
                "live" -> "correct"
            )

            result.isValid must_== true

            result("identity") must_== Some("unchanged")
            result("live") must_== Some("filtered")

            result.original("identity") must_== Some("unchanged")
            result.original("live") must_== Some("correct")
        }

        "validate a map of invalid values" in {
            val result = form.process(
                "identity" -> "unchanged",
                "live" -> "wrong"
            )

            result.isValid must_== false

            result("identity") must_== Some("unchanged")
            result("live") must_== Some("fail")

            result.original("identity") must_== Some("unchanged")
            result.original("live") must_== Some("wrong")
        }

    }

}


