/**
 * Definitions for forms
 */

package com.roundeights.vfunk.form

import com.roundeights.vfunk.validate.Validator
import com.roundeights.vfunk.validate.{Result => ValidateResult}
import com.roundeights.vfunk.filter.Filter

import scala.collection.immutable.HashMap

/**
 * A field definition
 */
trait Field {

    /**
     * Returns the name of this field
     */
    def name(): String

    /**
     * Runs validation against this form
     */
    def validate ( value: String ): FieldResult

}

/**
 * A text field
 */
class TextField (
    val name: String, private val validator: Validator,
    private val filter: Filter
) extends Field {

    /**
     * Runs validation against this form
     */
    override def validate ( value: String ): FieldResult = {
        val filtered = filter.filter( value )
        FieldResult( this, value, filtered, validator.validate(filtered) )
    }

}

/**
 * The results from an individual field
 */
case class FieldResult (
    val field: Field, val original: String,
    val value: String, val valid: ValidateResult
)

/**
 * A form
 */
class Form ( fieldList: Traversable[Field] ) {

    /**
     * The list of fields mapped by their field name
     */
    val fields = {
        fieldList.foldLeft ( HashMap[String, Field]() ) {
            (accum, field) => accum + ((field.name, field))
        }
    }

    /**
     * Validates a map against this form
     */
    // def validate ( Map[String,String] ) = {}

}

/**
 * The results of a validation run
 */
abstract class FormResults (
    private val fields: HashMap[String,FieldResult]
) {

    /**
     * Returns whether this form is valid
     */
    def isValid (): Boolean

    /**
     * Returns the value of a field
     */
    def value ( field: String ): String

    /**
     * Returns the original value from a form
     */
    def original ( field: String ): String

}


