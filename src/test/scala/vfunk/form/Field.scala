package test.roundeights.vfunk

import org.specs2.mutable._

import com.roundeights.vfunk._
import com.roundeights.vfunk.validate._
import com.roundeights.vfunk.filter._

class FieldTest extends Specification  {

    "A Field" should {

        val field = TextField(
            "fieldName",
            new Callback( _ match {
                case "correct" => "filtered"
                case _ => "fail"
            } ),
            new In( "filtered" )
        )

        "Validate and filter a passing value" in {
            val result = field.process( "correct" )

            result.field must_== field
            result.name must_== "fieldName"
            result.original must_== "correct"
            result.value must_== "filtered"
            result.isValid must_== true
            result.errors must_== List()
            result.firstError must_== None
        }

        "Validate and filter a failing value" in {
            val result = field.process( "wrong" )

            result.field must_== field
            result.name must_== "fieldName"
            result.original must_== "wrong"
            result.value must_== "fail"
            result.isValid must_== false
            result.errors must_== List( Err("OPTION", "Invalid Option") )
            result.firstError must_== Some( Err("OPTION", "Invalid Option") )
        }

        "Produce an Either" in {
            field.process("correct").either must_== Right("filtered")
            field.process("wrong").either must_== Left(
                Validated( "fail", List(Err("OPTION", "Invalid Option")) )
            )
        }

        "Produce an Option" in {
            field.process("correct").option must_== Some("filtered")
            field.process("wrong").option must_== None
        }

    }

}


