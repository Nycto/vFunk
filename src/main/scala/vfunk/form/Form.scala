package com.roundeights.vfunk

/**
 * A companion for the Form class
 */
object Form {

    /**
     * Creates a form from a list of fields
     */
    def apply ( fields: Traversable[Field] ): Form = new Form( fields )

    /**
     * Creates a form from a list of fields
     */
    def apply ( fields: Field* ): Form = new Form( fields )

}

/**
 * A form
 */
case class Form ( val fields: Map[String, Field] ) extends Traversable[Field] {

    /**
     * Creates a form from a list of fields
     */
    def this ( fields: Traversable[Field] ) = this(
        fields.foldLeft ( Map[String, Field]() ) {
            (accum, field) => accum + ((field.name, field))
        }
    )

    /**
     * Creates a form from a list of fields
     */
    def this ( fields: Field* ) = this( fields )

    /**
     * Creates a new form including a new field
     */
    def + ( field: Field ) = new Form( fields + ((field.name, field)) )

    /**
     * Validates a map against this form
     */
    def process ( values: Map[String,String] ): FormResults = {
        fields.foldLeft( FormResults() ) {
            (accum, pair) => accum + ((
                pair._1,
                pair._2.process( values.getOrElse( pair._1, "" ) )
            ))
        }
    }

    /**
     * Validates a list of tuples
     */
    def process ( values: (String, String)* ): FormResults
        = process( Map( values:_* ) )

    /** {@inheritDoc} */
    def foreach[U] ( callback: Field => U ): Unit
        = fields.foreach( pair => callback( pair._2 ) )

}

/**
 * The results of a validation run
 */
case class FormResults (
    val results: Map[String,FieldResult] = Map()
) extends Traversable[FieldResult] {

    /** {@inheritDoc} */
    def foreach[U] ( callback: FieldResult => U ): Unit
        = results.foreach( value => callback( value._2 ) )

    /**
     * Adds a new result to thie map
     */
    def + ( newElem: (String, FieldResult) ): FormResults
        = FormResults( results + newElem )

    /**
     * Returns whether this form is valid
     */
    def isValid: Boolean = results.forall( result => result._2.isValid )

    /**
     * Returns the value of a field
     */
    def apply ( field: String ): Option[String]
        = results.get( field ).map( _.value )

    /**
     * Returns the original value from a form
     */
    def original ( field: String ): Option[String]
        = results.get( field ).map( _.original )

}


