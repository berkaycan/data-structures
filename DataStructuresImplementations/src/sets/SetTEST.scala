package sets

/**
  * Created by berkay on 26.12.2016.
  */
object SetTEST extends App {
  def withTimer[T](f: => T) = {
    val startTime = System.currentTimeMillis
    val result = f
    val timeSpent = System.currentTimeMillis - startTime
    println("Operation took: " + timeSpent + " milliseconds!")
    result
  }

 val mutableset = MutableHashSet[Int]()
  withTimer {
    for (i <- 0 to 1000000)
      mutableset.add(i)
  }

  var immutableset = ImmutableHashSet[Int]()
  withTimer {
    for (i <- 0 to 1000000)
      immutableset = immutableset.add(i)
  }
}
