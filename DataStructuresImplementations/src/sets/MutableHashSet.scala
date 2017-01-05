package sets
import lists.MutableLinkedList
/**Simple mutable hashset implementation.**/
class MutableHashSet[A] {
  var size: Int = 0
  var bucket = Array.fill[MutableLinkedList[A]](10009)(MutableLinkedList[A]())
  def computeHash(key: A): Int = key.hashCode() % 10009
  def add(data: A): Unit = if(!bucket(computeHash(data)).contains(data)) {bucket(computeHash(data)).add(data); size +=1}
  override def toString: String = bucket.filter(list => !list.isEmpty).mkString(",")
  def contains(data: A): Boolean = bucket(computeHash(data)).contains(data)
  def foreach[U](f: A => U): Unit = bucket.foreach(list => list.foreach(f))
}

object MutableHashSet {
  def apply[A](values: A*): MutableHashSet[A] = {
    val set = new MutableHashSet[A]()
    values.foreach(elem => set.add(elem))
    set
  }
}