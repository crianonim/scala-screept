import org.scalatest.FunSuite
import site.jans.screept
import site.jans.screept._
import scala.collection.mutable
class ScreeptSuite extends FunSuite {
  val ops = Screept.getCoreOperators()
  // just so I know how to use
  // test("An empty Set should have size 0") {
  //   assert(Set.empty.size == 0)
  // }

  // test("Invoking head on an empty Set should produce NoSuchElementException") {
  //   assertThrows[NoSuchElementException] {
  //     Set.empty.head
  //   }
  // }

  test("toBoolean should work with all possibilities") {
    assert(!Screept.toBoolean(0))
    assert(!Screept.toBoolean(0.0))
    assert(!Screept.toBoolean(""))
    assert(!Screept.toBoolean(null))
    assert(Screept.toBoolean(1))
    assert(Screept.toBoolean("value"))
  }

  test("test MathOperators operators") {
    val eval = Screept.evaluate(Screept.getCoreOperators())(
      mutable.Map[String, String]()
    ) _
    assert(eval("3 2 + 5 =") == "1")
    assert(eval("3 -2 + 1 =") == "1")
    assert(eval("-3 -2 + -5 =") == "1")
    assert(eval("-3 -2 - -1 =") == "1")
    assert(eval("-10 -2 / 5 =") == "1")
    assert(eval("-10 4 / -2.5 =") == "1")
    assert(eval("-10 -3 * 30 =") == "1")
    assert(eval("-10 2 * -20 =") == "1")
    assert(eval("10 5 >") == "1")
    assert(eval("10 10.0 >") == "0")
    assert(eval("10 5 <") == "0")
    assert(eval("10 10.0 <") == "0")
    assert(eval("10 5 >=") == "1")
    assert(eval("10 10.0 >=") == "1")
    assert(eval("10 5 <=") == "0")
    assert(eval("10 10.0 <=") == "1")
  }

  test("test BasicOperators operators") {
    val eval = Screept.evaluate(Screept.getCoreOperators())(
      mutable.Map[String, String]()
    ) _
    assert(eval("Jan name := name =") == "1")
    assert(eval("Jan name := name Jan =") == "1")
    assert(eval("True False 1 1 = ?") == "True")
    assert(eval("True False 1 2 = ?") == "False")
    assert(eval("True False ; 1 2 = ? ;") == "False")
    assert(eval("True False ( 1 2 = ) ? ") == "False")
    assert(eval("Jan name := name DEBUG =") == "1")
    assert(eval("Jan name := ; 39 age := ; DEBUG age") == "39")
    assert(eval("Jan name := name PRINT") == "Jan")
  }

  test("test LogicOperators operators") {
    val eval = Screept.evaluate(Screept.getCoreOperators())(
      mutable.Map[String, String]()
    ) _
    assert(eval("1 1 &") == "1")
    assert(eval("0 1 &") == "0")
    assert(eval("0 0 &") == "0")
    assert(eval("1 1 |") == "1")
    assert(eval("1 0 |") == "1")
    assert(eval("0 0 |") == "0")
    assert(eval("0.0 !") == "1")
    assert(eval("1.0 !") == "0")
    assert(eval("1 1 & 0 1 & |") == "1")
    assert(eval("1 1 &  ( 0 1 & ) |") == "1")
    assert(eval("DEBUG 1") == "1")
  }

  test("interpolation works") {
    val ctx =
      mutable.Map[String, String]("name" -> "Jan", "turn" -> "2", "a" -> "100")
    val inter = Screept.interpolate(ops)(ctx) _
    assert(inter("Hello,#{name} it's #{turn}") == "Hello,Jan it's 2")
    assert(
      inter("Hello,#{'Lucas is stupid' 'Kasia is not' a 50 > ?} it's #{turn}") == "Hello,Lucas is stupid it's 2"
    )
  }

  test("StringOperators") {
    val ctx = mutable.Map[String, String](
      "name" -> "Jan",
      "turn" -> "2",
      "a" -> "100",
      "bob_name" -> "Roberto",
      "bob_money" -> "100",
      "npc" -> "bob"
    )
    val eval = Screept.evaluate(Screept.getCoreOperators())(ctx) _
    assert(eval("Jan ' ' CONCAT Skowronski CONCAT b := b") == "Jan Skowronski")
    assert(
      eval("50 npc _ CONCAT money CONCAT := ; 30 g := ; bob_money g +") == "80.0"
    )
    assert(
      eval("50 npc _ money CONCAT3 := ; 30 g := ; bob_money g +") == "80.0"
    )
  }

  test("ObjectOperators") {
    val ctx = mutable.Map[String, String](
      "name" -> "Jan",
      "turn" -> "2",
      "a" -> "100",
      "bob_name" -> "Roberto",
      "bob_money" -> "100",
      "npc" -> "bob",
      "zero" -> "0"
    )
    val eval = Screept.evaluate(Screept.getCoreOperators())(ctx) _
    assert(eval("50 npc money OBJ := ; 30 g := bob_money g +") == "80.0")
    assert(eval("bob_dupa VALZERO") == "0.0")
    assert(eval("bob_money VALZERO") == "50")
    assert(eval("bob_money ZERO bob_money") == "0")
    assert(eval("bob_money ONE bob_money") == "1")
    assert(eval("0 ?EMPTY") == "1")
    assert(eval("0.0 ?EMPTY") == "1")
    assert(eval("zero ?EMPTY") == "1")
    assert(eval("nonexistent ?EMPTY") == "1")
    assert(eval("bob money OBJ ?EMPTY") == "0")
  }

}
