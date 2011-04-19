package test.scala.vfunk.filter

/**
 * A helper class for testing what a validator passes for
 */
object FilterHelper {
    /**
     * A helper method that builds a string from a list of characters
     */
    def build( list: Range* ) = {
        list.foldLeft[Traversable[Int]] ( Nil ) ( _ ++ _ )
            .map( _.asInstanceOf[Char] )
            .foldLeft("")(_ + _)
    }
}

