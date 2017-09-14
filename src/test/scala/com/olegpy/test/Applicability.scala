package com.olegpy.test

import com.olegpy.forwarders
import utest._


object Applicability extends TestSuite {
  val tests = Tests {
    "Ok" - {
      "trait without companion or subtypes" - {
        @forwarders trait Foo[A]
        @forwarders trait Bar
      }
    }

    "Not ok" - {
      "trait extending traits" - {
        compileError("trait Sup; @forwarders trait Sub[A] extends Sup")
        compileError("trait SupK[F[_]]; @forwarders trait SubK[F[_]] extends SupK[F]")
      }

      "abstract type" - {
        compileError("@forwarders type NewType")
        compileError("@forwarders type NewType1[A]")
      }

      "type alias" - {
        compileError("@forwarders type NewType = List[Int]")
        compileError("@forwarders type NewType1[A] = List[A]")
      }

      "trait with companion" - {
        compileError("@forwarders trait Comp; object Comp")
        compileError("@forwarders trait Comp[F[_]]; object Comp")
      }

      "any class" - {
        compileError("@forwarders abstract class Yeek")
        compileError("@forwarders case class Hill()")
      }

      "object" - {
        compileError("@forwarders object Me")
        compileError("@forwarders case object Him")
      }
    }
  }
}
