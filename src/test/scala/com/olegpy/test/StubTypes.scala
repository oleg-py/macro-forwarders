package com.olegpy.test

import scala.language.higherKinds

import com.olegpy.forwarders


trait StubTypes {
  type String1[A] = String

  @forwarders trait Foo[M[_], A] {
    val valueA, valueB: A
    def paramless: A
    def emptyParams(): A
    def typeParam[B]: M[B]
    def someParams(a: A): M[Unit]
    def emptyImplicitParams(implicit di: DummyImplicit): A
    def nonEmptyImplicitParams(a: A)(implicit di: DummyImplicit): M[A]
    def manyParamLists[B, C](a: A)(b: B, c: C)(implicit di: DummyImplicit): M[Any]
  }

  /*_*/
  protected implicit val foo: Foo[String1, String] = new Foo[String1, String] {
    val valueA = "valueA"
    val valueB = "valueB"
    def paramless = "paramless"
    def emptyParams() = "emptyParams()"
    def typeParam[B] = "typeParam[B]"
    def someParams(a: String) = "someParams(a: A)"
    def emptyImplicitParams(implicit di: DummyImplicit) = "emptyImplicitParams"
    def nonEmptyImplicitParams(a: String)(implicit di: DummyImplicit) =
      "nonEmptyImplicitParams(a: A)"
    def manyParamLists[B, C](a: String)(b: B, c: C)(implicit di: DummyImplicit) =
      "manyParamLists[B, C](a: A)(b: B, c: C)"
  }
}
