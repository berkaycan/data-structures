package maps

class MutableHashMap[K,V](initialCapacity: Int) {
  class Node(var hash: Int, var key: K, var value: V, var next: Node)
  def this() = this(16)
  private val defaultCapacity = 16
  private var currentCapacity = if(initialCapacity <= 0) defaultCapacity else initialCapacity
  private val maxLoadFactor = 0.75f
  private var threshold = currentCapacity*maxLoadFactor
  private var size = 0
  private var bucket: Array[Node] = new Array[Node](currentCapacity)

  def hash(key: K): Int = {
    return key.hashCode
  }
  def isEmpty(): Boolean = size == 0
  private def indexFor(h: Int): Int = return (h >>> 1) & (currentCapacity -1)
  def set(key: K, value: V): Unit = {
    val h = hash(key)
    val i = indexFor(h)
    var node: Node = bucket(i)
    while(node!=null && node.key != key) {
      node = node.next
    }
    if(node == null){
      val e = new Node(h, key, value, bucket(i))
      bucket(i) = e
      size+=1
      if(size > threshold) resize(currentCapacity*2)
    } else node.value = value
  }
  private def resize(newCapacity: Int): Unit ={
    var i = currentCapacity - 1
    currentCapacity = newCapacity
    val oldBucket = bucket
    bucket = new Array[Node](newCapacity)
    while(i >= 0) {
      var e = oldBucket(i)
      while(e!=null) {
        val next = e.next
        val i = indexFor(e.hash)
        e.next = bucket(i)
        bucket(i) = e
        e = next
      }
      i -= 1
    }
    threshold = newCapacity * maxLoadFactor
  }
  def get(key: K): Option[V] = {
    var x = bucket(indexFor(hash(key)))
    while(x!=null && x.key != key) {
      x = x.next
    }
    if(x == null) None
    else Option(x.value)
  }
}

object MutableHashMap {
  def apply[K,V](): MutableHashMap[K,V] = new MutableHashMap[K,V]()
  def apply[K,V](kv: (K,V)*): MutableHashMap[K,V] = {
    val hm = new MutableHashMap[K,V]()
    for (elem <- kv) {
      hm.set(elem._1,elem._2)
    }
    hm
  }
}