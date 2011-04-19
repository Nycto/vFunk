/**
 * Specific Data type filters
 */

package main.scala.vfunk.filter

import scala.collection.immutable.HashSet

/**
 * Filters out invalid email address characters
 */
class EMail extends Characters (
    HashSet[Char]() ++ ('a' to 'z') ++ ('A' to 'Z') ++
        ('0' to '9') ++ ("!#$%&'*+-/=?^_`{|}~@".toList)
) {}

