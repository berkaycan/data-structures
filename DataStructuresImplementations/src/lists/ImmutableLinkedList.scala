package lists

sealed abstract class ImmutableLinkedList[A] {
  def getSuper: ImmutableLinkedList[A] = this
  def head: A
  def tail: ImmutableLinkedList[A]
  def isEmpty: Boolean
  def add(data: A): ImmutableLinkedList[A]
  def toString: String
  def contains(value: A): Boolean = {
    var self = getSuper
    while(!self.isEmpty){
      if(self.head == value) return true
      else self = self.tail
    }
    false
  }
  def foreach[U](f: (A) => U): Unit = {
    var self = getSuper
    while(!self.isEmpty) {
      f(self.head)
      self = self.tail
    }
  }
}

class Cons[A] (data: A, next: ImmutableLinkedList[A]) extends ImmutableLinkedList[A]{
  override def head: A = data
  override def tail: ImmutableLinkedList[A] = next
  override def isEmpty: Boolean = false
  override def add(value: A): ImmutableLinkedList[A] = new Cons(value, this)
  override def toString(): String = {
    var string = ""
    var self = getSuper
    while(!self.isEmpty) {
      string += self.head.toString + ","
      self = self.tail
    }
    return string.substring(0, string.length - 1)
  }
}

class Nul[A] extends ImmutableLinkedList[A]{
  override def isEmpty: Boolean = true
  override def head = throw new IllegalArgumentException("List has no head!")
  override def tail = throw new IllegalArgumentException("List has no tail!")
  override def add(value: A): ImmutableLinkedList[A] = new Cons(value, this)
  override def toString(): String = ""
}

object ImmutableLinkedList {
  def apply[A](): ImmutableLinkedList[A] = new Nul
  def apply[A](values: A*): ImmutableLinkedList[A] = {
    var list = ImmutableLinkedList[A]()
    for(v <- values) {
      list = list.add(v)
    }
    list
  }
}