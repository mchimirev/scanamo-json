package io.github.howardjohn

import java.io.Serializable

import io.circe.{Json, JsonNumber, JsonObject}
import org.scanamo.{DynamoArray, DynamoObject, DynamoValue}
import cats.implicits._

package object scanamo {

  trait Folder[X] extends Serializable {
    def onNull: X
    def onBoolean(value: Boolean): X
    def onNumber(value: String): X
    def onString(value: String): X
    def onArray(value: DynamoArray): X
    def onObject(value: DynamoObject): X
  }
  implicit class ExtendedDynamoValue(val value: DynamoValue) extends AnyVal {
    def foldWith[T](folder: Folder[T]): T =
      value.asString
        .map(folder.onString)
        .orElse(value.asNumber.map(folder.onNumber))
        .orElse(value.asNull.map(_ => folder.onNull))
        .orElse(value.asBoolean.map(folder.onBoolean))
        .orElse(value.asObject.map(folder.onObject))
        .orElse(value.asArray.map(folder.onArray))
        //TODO: we expect that all types are covered
        .get
  }
}
