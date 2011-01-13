import org.scalatest.{ FunSuite, OneInstancePerTest }
import scala.collection.mutable.ListBuffer
import events._

class MySuite extends FunSuite with OneInstancePerTest {
  lazy val buy = new ImperativeEvent[Int]
  lazy val sell = new ImperativeEvent[Int]

  val log = ListBuffer[(String, Any)]()

  def logger(name: String) = ((v: Any) => {
    log += ((name, v))
    ()
  })

  def assertLogged(pairs: (String, Any)*) {
    assert(pairs.toList === log.toList)
  }

  def assertLogged(name: String)(values: Any*) {
    assert(values.toList === log.toList.filter(_._1 == name).map(_._2))
  }

  test("imparative events") {
    buy += logger("buy")
    sell += logger("sell")

    buy(4)
    buy(3)
    sell(5)
    buy(1)

    assertLogged(("buy", 4), ("buy", 3), ("sell", 5), ("buy", 1))
  }

  test("OR events") {
    val order = buy || sell
    order += logger("order")

    buy(4)
    buy(3)
    sell(5)
    buy(1)

    assertLogged(("order", 4), ("order", 3), ("order", 5), ("order", 1))
  }

  test("single right cross join") {
    val buyJoinSell = buy join (sell, 10, (a: Int, b: Int) => (a, b))
    buyJoinSell += logger("bs")

    sell(1)
    buy(2)
    buy(3)
    buy(4)

    assertLogged("bs")((2, 1), (3, 1), (4, 1))
  }

  test("single left cross join") {
    val buyJoinSell = buy join (sell, 10, (a: Int, b: Int) => (a, b))
    buyJoinSell += logger("bs")

    buy(1)
    sell(2)
    sell(3)
    sell(4)

    assertLogged("bs")((1, 2), (1, 3), (1, 4))
  }

  test("single right reverse cross join") {
    val buyJoinSell = buy join (sell, 10, (a: Int, b: Int) => (a, b))
    buyJoinSell += logger("bs")

    buy(2)
    buy(3)
    buy(4)
    sell(1)

    assertLogged("bs")((2, 1), (3, 1), (4, 1))
  }

  test("single left reverse cross join") {
    val buyJoinSell = buy join (sell, 10, (a: Int, b: Int) => (a, b))
    buyJoinSell += logger("bs")

    sell(2)
    sell(3)
    sell(4)
    buy(1)

    assertLogged("bs")((1, 2), (1, 3), (1, 4))
  }

  test("double cross join") {
    val buyJoinSell = buy join (sell, 10, (a: Int, b: Int) => (a, b))
    buyJoinSell += logger("bs")

    buy(1)
    buy(2)
    sell(3)
    sell(4)

    assertLogged("bs")((1, 3), (2, 3), (1, 4), (2, 4))
  }
}
