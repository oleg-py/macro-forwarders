package com.olegpy.test

import utest._


object Utilities extends TestSuite with StubTypes {
  val tests = Tests {
    "apply[..] summon method" - {
      assert(Foo[String1, String] eq foo)
    }
  }
}
