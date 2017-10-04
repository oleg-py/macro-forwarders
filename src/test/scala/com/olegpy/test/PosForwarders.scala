package com.olegpy.test

import utest._

object PosForwarders extends TestSuite with StubTypes {
  val tests = Tests {
    "val" - {
      assert(Foo.ops.valueA == foo.valueA)
      assert(Foo.ops.valueB == foo.valueB)
    }

    "def no-args" - {
      assert(Foo.ops.paramless[String1, String] == foo.paramless)
      assert(Foo.ops.paramless == foo.paramless)
    }

    "def ()-arg" - {
      assert(Foo.ops.emptyParams() == foo.emptyParams())
    }

    "def type-param" - {
      assert(Foo.ops.typeParam[String1, String, Unit] == foo.typeParam[Unit])
    }

    "def single-param" - {
      assert(Foo.ops.someParams("3") == foo.someParams("3"))
    }

    "def single implicit param" - {
      assert(Foo.ops.emptyImplicitParams == foo.emptyImplicitParams)
    }

    "def normal param & implicit param" - {
      assert(Foo.ops.nonEmptyImplicitParams("4") == foo.nonEmptyImplicitParams("4"))
    }

    "def multiple param lists" - {
      assert(Foo.ops.manyParamLists("2")(3, 4) == foo.manyParamLists("2")(3, 4))
    }
  }
}
