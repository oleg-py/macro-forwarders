package com.olegpy.test

import utest._

object PosForwarders extends TestSuite with StubTypes {
  val tests = Tests {
    "val" - {
      assert(Foo.valueA == foo.valueA)
      assert(Foo.valueB == foo.valueB)
    }

    "def no-args" - {
      assert(Foo.paramless[String1, String] == foo.paramless)
      assert(Foo.paramless == foo.paramless)
    }

    "def ()-arg" - {
      assert(Foo.emptyParams() == foo.emptyParams())
    }

    "def type-param" - {
      assert(Foo.typeParam[String1, String, Unit] == foo.typeParam[Unit])
    }

    "def single-param" - {
      assert(Foo.someParams("3") == foo.someParams("3"))
    }

    "def single implicit param" - {
      assert(Foo.emptyImplicitParams == foo.emptyImplicitParams)
    }

    "def normal param & implicit param" - {
      assert(Foo.nonEmptyImplicitParams("4") == foo.nonEmptyImplicitParams("4"))
    }

    "def multiple param lists" - {
      assert(Foo.manyParamLists("2")(3, 4) == foo.manyParamLists("2")(3, 4))
    }
  }
}
