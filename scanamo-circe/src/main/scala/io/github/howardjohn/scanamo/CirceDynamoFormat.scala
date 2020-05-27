package io.github.howardjohn.scanamo

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json, JsonNumber, JsonObject}
import org.scanamo.{DynamoArray, DynamoFormat, DynamoObject, DynamoReadError, DynamoValue, TypeCoercionError}

import scala.collection.JavaConverters._
import cats.implicits._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

object CirceDynamoFormat {
  implicit def format[T: Encoder: Decoder]: DynamoFormat[T] = new DynamoFormat[T] {
    override def read(dv: DynamoValue): Either[DynamoReadError, T] = {
      val j = dv.foldWith {
        new Folder[Json] {
          override def onNull: Json = Json.Null
          override def onBoolean(value: Boolean): Json = Json.fromBoolean(value)
          override def onNumber(value: String): Json = Json.fromJsonNumber(JsonNumber.fromDecimalStringUnsafe(value))
          override def onString(value: String): Json = Json.fromString(value)
          override def onArray(value: DynamoArray): Json = {
            val arr = value.asArray
              .map(_.map(_.foldWith(this)))
              .orElse(value.asNumericArray.map(_.map(this.onNumber)))
              .orElse(value.asStringArray.map(_.map(this.onString)))
              .getOrElse(List.empty)
            Json.fromValues(arr)
          }
          override def onObject(value: DynamoObject): Json =
            Json.fromJsonObject(
              JsonObject.fromMap(
                value.keys
                  .zip(value.values)
                  .map {
                    case (k, v) => k -> v.foldWith(this)
                  }
                  .toMap))
        }
      }
      j.as[T] leftMap TypeCoercionError.apply
    }

    override def write(t: T): DynamoValue = {
      val av = t.asJson.foldWith {
        new Json.Folder[AttributeValue] {
          override def onNull: AttributeValue = AttributeValue.builder().nul(true).build()
          override def onBoolean(value: Boolean): AttributeValue = AttributeValue.builder().bool(value).build()
          override def onNumber(value: JsonNumber): AttributeValue = AttributeValue.builder().n(value.toString).build()
          override def onString(value: String): AttributeValue = AttributeValue.builder().s(value).build()
          override def onArray(value: Vector[Json]): AttributeValue =
            AttributeValue.builder().l(value.map(j => j.foldWith(this)).asJavaCollection).build()
          override def onObject(value: JsonObject): AttributeValue =
            AttributeValue.builder().m(value.toMap.map { case (k, v) => k -> v.foldWith(this) }.asJava).build()
        }
      }
      DynamoValue.fromAttributeValue(av)
    }
  }
}
