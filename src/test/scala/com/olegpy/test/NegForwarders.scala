package com.olegpy.test

import com.olegpy.forwarders
import utest._


class NegForwarders extends TestSuite {
  val tests = Tests {
    "private / protected methods" - {
      @forwarders trait Foo[A] {
        private def privateMethod = 0
        protected def protectedMethod = 0
      }
      implicit val foo = new Foo[Int] { }

      * - compileError("""Foo.protectedMethod""")
      * - compileError("""Foo.privateMethod""")
    }
  }
}
