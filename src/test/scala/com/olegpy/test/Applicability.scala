package com.olegpy.test

import com.olegpy.forwarders
import utest._


object Applicability extends TestSuite {
  val tests = Tests {
    "Ok" - {
      "trait without companion or subtypes" - {
        * - { @forwarders trait Foo[A] }

        * - { @forwarders trait Bar }
      }

      "trait with companion" - {
        * - { @forwarders trait Comp; object Comp }
        * - { @forwarders trait Comp[F[_]]; object Comp }
      }

      "class without explicit supertypes" - {
        * - { @forwarders abstract class Yeek }
        * - { @forwarders case class Hill() }
      }

      "trait extending other traits" - {
        * - {
          trait Sup
          @forwarders trait Sub[A] extends Sup
        }
        * - {
          trait SupK[F[_]]
          @forwarders trait SubK[F[_]] extends SupK[F]
        }
      }
    }

    "Not ok" - {
      "abstract type" - {
        * - compileError("@forwarders type NewType")
        * - compileError("@forwarders type NewType1[A]")
      }

      "type alias" - {
        * - compileError("@forwarders type NewType = List[Int]")
        * - compileError("@forwarders type NewType1[A] = List[A]")
      }

      "lone object" - {
        * - compileError("@forwarders object Me")
        * - compileError("@forwarders case object Him")
      }

      "companion with `ops` field" - {
        compileError(
          """
             @forwarders trait Foo
             object Foo { val ops = 42 }
          """)
      }

      "shadowed type parameters" - {
        compileError("""@forwarders trait Foo[A]{ def mkA[A]: A = ??? }""")
      }
    }
  }
}
