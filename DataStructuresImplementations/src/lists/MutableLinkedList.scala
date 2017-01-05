package lists

/**
  * Created by berkay on 26.12.2016.
  */
sealed trait LinkedList[A] {
  def isEmpty: Boolean
  def add(value: A): Unit
  def toString: String
  def contains(value: A): Boolean
  def foreach[U](f: A => U): Unit
  def length(): Int
}
class MutableLinkedList[A] extends LinkedList[A] {
  class Node[A](var value: A) {
    var next: Node[A] = _
    override def toString: String = value.toString
  }
  private var head: Node[A] = _
  private var size: Int = 0

  override def length(): Int = size
  override def isEmpty(): Boolean = head eq null
  override def add(value: A): Unit = {
    val node = new Node(value)
    node.next = head
    head = node
    size += 1
  }
  override def toString: String = {
    if (isEmpty) return ""
    var temp = head
    var s = ""
    while (temp ne null){
      s += temp.value + ","
      temp = temp.next
    }
    s.substring(0,s.length-1)
  }
  override def contains(value: A): Boolean = {
    var temp = head
    while(temp!=null) {
      if (temp.value == value) return true
      else temp = temp.next
    }
    false
  }
  override def foreach[U](f: A => U): Unit = {
    var temp = head
    while(temp ne null){
      f(temp.value)
      temp = temp.next
    }
  }
}

object MutableLinkedList {
  def apply[A](values: A*): MutableLinkedList[A] = {
    val list = new MutableLinkedList[A]()
    values.foreach(elem => list.add(elem))
    list
  }
}