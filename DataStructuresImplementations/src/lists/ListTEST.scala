package lists

/**
  * Created by berkay on 22.12.2016.
  */
object ListTEST extends App {

  def withTimer[T](f: => T) = {
    val startTime = System.currentTimeMillis
    val result = f
    val timeSpent = System.currentTimeMillis - startTime
    println("Operation took: " + timeSpent + " milliseconds!")
    result
  }

  val l = MutableLinkedList[Int]()
  withTimer{
    for (i <- 0 to 1000000)
      l.add(i)
  }

  var l2 = ImmutableLinkedList[Int]()
  withTimer{
    for (i <- 0 to 1000000)
      l2 = l2.add(i)
  }
}
