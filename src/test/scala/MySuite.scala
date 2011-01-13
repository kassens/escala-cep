import org.scalatest.{FunSuite, BeforeAndAfterEach}
import scala.collection.mutable.ListBuffer
import events._

class Stock {
  lazy val buy = new ImperativeEvent[Int]
  lazy val sell = new ImperativeEvent[Int]
}

class MySuite extends FunSuite with BeforeAndAfterEach {
  var log = ListBuffer[(String, Any)]()
  def logger(name : String) = (
    (v : Any) => {
      log += ((name, v))
      ()
    }
  )
  def assertLogged(pairs : (String, Any)*) {
    assert(pairs.toList === log.toList)
  }

  override def beforeEach() = {}
  
  override def afterEach() = {
    log.clear()
  }
  
  test("temp") {
    val x = logger("hi")
    x(1)
    x(3)
    assertLogged(("hi", 1), ("hi", 3))
  }
  
  test("imparative events") {
    val s = new Stock
    s.buy += logger("buy")
    s.sell += logger("sell")
    
    s.buy(4)
    s.buy(3)
    s.sell(5)
    s.buy(1)
    
    assertLogged(("buy", 4), ("buy", 3), ("sell", 5), ("buy", 1))
  }
  
  test("OR events") {
    val s = new Stock
    val order = s.buy || s.sell
    order += logger("order")
    
    s.buy(4)
    s.buy(3)
    s.sell(5)
    s.buy(1)
    
    assertLogged(("order", 4), ("order", 3), ("order", 5), ("order", 1))
  }
  
  test("single right cross join") {
    val s = new Stock
    val buyJoinSell = s.buy join (s.sell, (a : Int, b : Int) => (a, b))
    buyJoinSell += logger("bs")
    
    s.sell(1)
    s.buy(2)
    s.buy(3)
    s.buy(4)
    
    assertLogged(
      ("bs", (2,1)),
      ("bs", (3,1)),
      ("bs", (4,1)))
  }
  
  test("single left cross join") {
    val s = new Stock
    val buyJoinSell = s.buy join (s.sell, (a : Int, b : Int) => (a, b))
    buyJoinSell += logger("bs")
    
    s.buy(1)
    s.sell(2)
    s.sell(3)
    s.sell(4)
    
    assertLogged(
      ("bs", (1,2)),
      ("bs", (1,3)),
      ("bs", (1,4)))
  }
  
  test("single right reverse cross join") {
    val s = new Stock
    val buyJoinSell = s.buy join (s.sell, (a : Int, b : Int) => (a, b))
    buyJoinSell += logger("bs")
    
    s.buy(2)
    s.buy(3)
    s.buy(4)
    s.sell(1)
    
    assertLogged(
      ("bs", (2,1)),
      ("bs", (3,1)),
      ("bs", (4,1)))
  }
  
  test("single left reverse cross join") {
    val s = new Stock
    val buyJoinSell = s.buy join (s.sell, (a : Int, b : Int) => (a, b))
    buyJoinSell += logger("bs")
    
    s.sell(2)
    s.sell(3)
    s.sell(4)
    s.buy(1)
    
    assertLogged(
      ("bs", (1,2)),
      ("bs", (1,3)),
      ("bs", (1,4)))
  }
}
