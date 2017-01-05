package sets
import lists.ImmutableLinkedList
/**Simple immutable hashset implementation**/
sealed abstract class ImmutableHashSet[A] {
  def add(data: A): ImmutableHashSet[A]
  def toString: String
  def contains(data: A): Boolean
  def computeHash(key: A): Int = key.hashCode() % 10009
  def foreach[U](f: A => U): Unit
}
class Cons[A](newHashTable: Array[ImmutableLinkedList[A]]) extends ImmutableHashSet[A] {
  val myHashSet: Array[ImmutableLinkedList[A]] = newHashTable
  override def contains(data: A): Boolean = myHashSet(computeHash(data)).contains(data)
  override def add(data: A): ImmutableHashSet[A] = {
    val bucket:Array[ImmutableLinkedList[A]] = new Array[ImmutableLinkedList[A]](10009)
    val hash = computeHash(data)
    Array.copy(myHashSet, 0, bucket, 0, 10009)
    if (!bucket(hash).contains(data))
      bucket(hash) = bucket(hash).add(data)
    new Cons(bucket)
  }
  override def foreach[U](f: A => U): Unit = myHashSet.foreach(list => list.foreach(f))
  override def toString: String = myHashSet.filter(list => !list.isEmpty).mkString(",")
}

object ImmutableHashSet {
  def apply[A](): ImmutableHashSet[A] = new Cons(Array.fill[ImmutableLinkedList[A]](10009)(ImmutableLinkedList[A]()))
  def apply[A](values: A*): ImmutableHashSet[A] = {
    var set = ImmutableHashSet[A]()
    for(v <- values)
      set = set.add(v)
    set
  }
}