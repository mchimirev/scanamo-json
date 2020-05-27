package io.github.howardjohn.scanamo

import org.scalatest.{Assertion, EitherValues, FunSuite, Matchers}
import org.scanamo.DynamoFormat

trait DynamoFormatBehavior extends Matchers with EitherValues { this: FunSuite =>
  def dynamoFormatTest[T](parse: String => Either[Any, T])(format: DynamoFormat[T]): Unit = {
    def roundTrip(input: String, expected: String): Assertion = {
      val json = parse(input)
      val attribute = format.write(json.right.value).toAttributeValue
      val jsonResp = format.read(attribute)
      assert(expected === attribute.toString)
      assert(jsonResp === json)
    }

    test("empty map")(roundTrip("{}", "AttributeValue(SS=[], NS=[], BS=[], M={}, L=[])"))
    test("integer value")(
      roundTrip(
        """{"a":1}""",
        "AttributeValue(SS=[], NS=[], BS=[], M={a=AttributeValue(N=1, SS=[], NS=[], BS=[], M={}, L=[])}, L=[])"))
    test("string value")(
      roundTrip(
        """{"a":"b"}""",
        "AttributeValue(SS=[], NS=[], BS=[], M={a=AttributeValue(S=b, SS=[], NS=[], BS=[], M={}, L=[])}, L=[])"))
    test("bool value")(
      roundTrip(
        """{"a":true}""",
        "AttributeValue(SS=[], NS=[], BS=[], M={a=AttributeValue(SS=[], NS=[], BS=[], M={}, L=[], BOOL=true)}, L=[])"))
    test("null value")(
      roundTrip(
        """{"a":null}""",
        "AttributeValue(SS=[], NS=[], BS=[], M={a=AttributeValue(SS=[], NS=[], BS=[], M={}, L=[], NUL=true)}, L=[])"))
    test("map map")(
      roundTrip(
        """{"nested":{"a":1}}""",
        "AttributeValue(SS=[], NS=[], BS=[], M={nested=AttributeValue(SS=[], NS=[], BS=[], M={a=AttributeValue(N=1, SS=[], NS=[], BS=[], M={}, L=[])}, L=[])}, L=[])"
      ))
    test("int list value")(
      roundTrip(
        """{"a":[1,2,3]}""",
        "AttributeValue(SS=[], NS=[], BS=[], M={a=AttributeValue(SS=[], NS=[], BS=[], M={}, L=[AttributeValue(N=1, SS=[], NS=[], BS=[], M={}, L=[]), AttributeValue(N=2, SS=[], NS=[], BS=[], M={}, L=[]), AttributeValue(N=3, SS=[], NS=[], BS=[], M={}, L=[])])}, L=[])"
      ))
    test("string list value")(
      roundTrip(
        """{"a":["b","c","d"]}""",
        "AttributeValue(SS=[], NS=[], BS=[], M={a=AttributeValue(SS=[], NS=[], BS=[], M={}, L=[AttributeValue(S=b, SS=[], NS=[], BS=[], M={}, L=[]), AttributeValue(S=c, SS=[], NS=[], BS=[], M={}, L=[]), AttributeValue(S=d, SS=[], NS=[], BS=[], M={}, L=[])])}, L=[])"
      ))
    test("mixed values")(
      roundTrip(
        """{"a":1,"b":"value"}""",
        "AttributeValue(SS=[], NS=[], BS=[], M={a=AttributeValue(N=1, SS=[], NS=[], BS=[], M={}, L=[]), b=AttributeValue(S=value, SS=[], NS=[], BS=[], M={}, L=[])}, L=[])"
      ))
    test("mixed list") {
      roundTrip(
        """{"a":[1,"b",false,null]}""",
        "AttributeValue(SS=[], NS=[], BS=[], M={a=AttributeValue(SS=[], NS=[], BS=[], M={}, L=[AttributeValue(N=1, SS=[], NS=[], BS=[], M={}, L=[]), AttributeValue(S=b, SS=[], NS=[], BS=[], M={}, L=[]), AttributeValue(SS=[], NS=[], BS=[], M={}, L=[], BOOL=false), AttributeValue(SS=[], NS=[], BS=[], M={}, L=[], NUL=true)])}, L=[])"
      )
    }
    test("nested list") {
      roundTrip(
        """{"a":[1,[2,[3]]]}""",
        "AttributeValue(SS=[], NS=[], BS=[], M={a=AttributeValue(SS=[], NS=[], BS=[], M={}, L=[AttributeValue(N=1, SS=[], NS=[], BS=[], M={}, L=[]), AttributeValue(SS=[], NS=[], BS=[], M={}, L=[AttributeValue(N=2, SS=[], NS=[], BS=[], M={}, L=[]), AttributeValue(SS=[], NS=[], BS=[], M={}, L=[AttributeValue(N=3, SS=[], NS=[], BS=[], M={}, L=[])])])])}, L=[])"
      )
    }
    test("just bool")(roundTrip("true", "AttributeValue(SS=[], NS=[], BS=[], M={}, L=[], BOOL=true)"))
    test("just number")(roundTrip("1", "AttributeValue(N=1, SS=[], NS=[], BS=[], M={}, L=[])"))
    test("just string")(roundTrip("\"string\"", "AttributeValue(S=string, SS=[], NS=[], BS=[], M={}, L=[])"))
    test("just list")(
      roundTrip(
        "[1,2,3]",
        "AttributeValue(SS=[], NS=[], BS=[], M={}, L=[AttributeValue(N=1, SS=[], NS=[], BS=[], M={}, L=[]), AttributeValue(N=2, SS=[], NS=[], BS=[], M={}, L=[]), AttributeValue(N=3, SS=[], NS=[], BS=[], M={}, L=[])])"
      ))
  }
}
