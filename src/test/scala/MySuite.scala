import org.scalatest.FunSuite
import events._

class Stock {
  lazy val buy = new ImperativeEvent[Int]
  lazy val sell = new ImperativeEvent[Int]
  lazy val order = buy || sell
  
  var log = List[String]()
  
  buy += onBuy _
  sell += onSell _
  
  def onBuy(v : Int) = {
    log ::= "buy " + v
  }
  
  def onSell(v : Int) = {
    log ::= "sell " + v
  }
}

class MySuite extends FunSuite {
  test("imparative events") {
    val s = new Stock
    assert(s.log === Nil)
    s.buy(4)
    s.buy(3)
    s.sell(5)
    s.buy(1)
    assert(s.log.reverse.mkString(" ") === "buy 4 buy 3 sell 5 buy 1")
  }
  test("OR events") {
    val s = new Stock
    assert(s.log === Nil)
    var orders = 0
    var total_value : Int = 0
    s.order += ((v : Int) => {
      total_value += v
      orders += 1
    })
    s.buy(4)
    s.buy(3)
    s.sell(5)
    s.buy(1)
    assert(orders === 4)
    assert(total_value === 13)
    assert(s.log.reverse.mkString(" ") === "buy 4 buy 3 sell 5 buy 1")
  }
}