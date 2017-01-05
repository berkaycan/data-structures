package maps

/**
  * Created by berkay on 27.12.2016.
  */
object HashMapTEST extends App{

  def withTimer[T](f: => T) = {
    val startTime = System.currentTimeMillis
    val result = f
    val timeSpent = System.currentTimeMillis - startTime
    println("Operation took: " + timeSpent + " milliseconds!")
    result
  }
  val hashmap = MutableHashMap[Int,Int]()
  withTimer {
    for (i <- 0 to 5000000)
      hashmap.set(i, i)
  }
  withTimer {
    println(hashmap.get(1000000))
  }
}
