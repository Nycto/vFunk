package test.roundeights.vfunk

import org.specs2.mutable._

import com.roundeights.vfunk._

class FormTest extends Specification  {

    "A Form" should {

        val form = Form(
            TextField( "one" ),
            TextField(
                "two",
                Filter callback( _ match {
                    case "correct" => "filtered"
                    case _ => "fail"
                } ),
                Validate in( "filtered" )
            ),
            TextField( "three", Filter.numeric, Validate >= 0 )
        )

        val valid = form.process(
            "one" -> "unchanged",
            "two" -> "correct",
            "three" -> "123"
        )

        val invalid = form.process(
            "one" -> "unchanged",
            "two" -> "wrong",
            "three" -> "-5"
        )

        "Preserve the order of its fields" in {
            val fields = form.toList
            fields(0).name must_== "one"
            fields(1).name must_== "two"
            fields(2).name must_== "three"
        }

        "provide access to whether a form is valid" in {
            valid.isValid must_== true
            invalid.isValid must_== false
        }

        "provide access to the filtered values" in {
            valid("one") must_== "unchanged"
            valid("two") must_== "filtered"
            valid("three") must_== "123"

            invalid("one") must_== "unchanged"
            invalid("two") must_== "fail"
            invalid("three") must_== "-5"
        }

        "provide access to the optional filtered values" in {
            valid.get("one") must_== Some("unchanged")
            valid.get("two") must_== Some("filtered")
            valid.get("three") must_== Some("123")
            valid.get("four") must_== None

            invalid.get("one") must_== Some("unchanged")
            invalid.get("two") must_== Some("fail")
            invalid.get("three") must_== Some("-5")
            invalid.get("four") must_== None
        }

        "provide access to the original values" in {
            valid.original("one") must_== Some("unchanged")
            valid.original("two") must_== Some("correct")
            valid.original("three") must_== Some("123")

            invalid.original("one") must_== Some("unchanged")
            invalid.original("two") must_== Some("wrong")
            invalid.original("three") must_== Some("-5")
        }

        "provide access to list of errors produced" in {
            valid.errors must_== Nil

            invalid.errors must_== List(
                Err("GREATERTHANEQUALS", "Must be greater than or equal to 0"),
                Err("OPTION", "Invalid Option")
            )
        }

        "provide access to list of errors produced" in {
            valid.errors must_== Nil

            invalid.errors must_== List(
                Err("GREATERTHANEQUALS", "Must be greater than or equal to 0"),
                Err("OPTION", "Invalid Option")
            )
        }

        "provide access to first error" in {
            valid.firstError must_== None
            invalid.firstError must_== Some(
                Err("GREATERTHANEQUALS", "Must be greater than or equal to 0")
            )
        }

        "provide access to first error message" in {
            valid.firstMessage must_== None
            invalid.firstMessage must_== Some(
                "Must be greater than or equal to 0"
            )
        }

        "provide access to the first invalid field result" in {
            valid.firstInvalid must_== None
            invalid.firstInvalid must_== Some( invalid.results("two") )
        }

        "Provide a map of error messages indexed by field" in {
            valid.fieldErrors must_== Map()
            invalid.fieldErrors must_== Map(
                "two" -> List("Invalid Option"),
                "three" -> List("Must be greater than or equal to 0")
            )
        }
    }

}


