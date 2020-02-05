import java.util.{Comparator, NoSuchElementException}
import java.util.function.{Consumer, Predicate}

class SCycleList[T] {
  implicit class Node(d: T) {
    var next: SCycleList[T]#Node = this
    var prev: SCycleList[T]#Node = this
    var data: T = d

    def unlink(): Node = {
      this.next.prev = this.prev
      this.prev.next = this.next
      this.next = this
      this.prev = this
      this
    }
  }

  var head: SCycleList[T]#Node = _
  var count = 0

  def is_empty: Boolean = head == null

  def length: Int = count

  def push_back(data: T): SCycleList[T] = {
    var n = Node(data)
    if (is_empty)
      head = n
    else {
      n.prev = head.prev
      n.next = head
      head.prev.next = n
      head.prev = n
    }
    count += 1
    this
  }

  def push_front(data: T): SCycleList[T] = {
    push_back(data)
    head = head.prev
    this
  }

  def node(vidx: Int): SCycleList[T]#Node = {
    var idx = vidx
    if (is_empty || idx >= count || idx < 0) return null
    var n = head
    while (idx > 0) {
      n = n.next
      idx -= 1
    }
    n
  }

  def at(idx: Int): Option[T] = {
    val n = node(idx)
    if (n == null) return null
    Option(n.data)
  }

  def remove(idx: Int): Option[T]  = {
    val n = node(idx)
    if (n == null) return null
    count -= 1
    if (head eq n) head = n.next
    if (head eq n) head = null
    n.unlink()
    Option(n.data)
  }

  def insert(el: T, idx: Int): Boolean = {
    val prev = node(idx - 1)
    if (prev == null) return false
    val q = new Node(el)
    q.next = q
    q.prev = q
    count += 1
    if (is_empty) {
      head = q
      return true
    }
    q.next = prev.next
    q.prev = prev
    prev.next = q
    q.next.prev = q
    if (idx == 0) head = q
    true
  }

  def pop_back: Option[T] = {
    if (is_empty) return null
    val cur = head.prev
    count -= 1
    if (head eq cur) head = cur.next
    if (head eq cur) head = null
    cur.unlink()
    Option(cur.data)
  }

  def pop_front: Option[T] = {
    if (is_empty) return null
    val cur = head
    count -= 1
    head = head.next
    if (head eq cur) head = null
    cur.unlink()
    Option(cur.data)
  }

  def for_each(action: Consumer[T]): Unit = {
    var cur = head
    var i = 0
    while ( {
      i < count
    }) {
      action.accept(cur.data)
      cur = cur.next
      i += 1
    }
  }

    def filter(p: Predicate[T]) = {
      var cur = head
      val ret = new SCycleList[T]
      var i = 0
      while ( {
        i < count
      }) {
        if (p.test(cur.data))
          ret.push_back(cur.data)
        i += 1
        cur = cur.next
      }
      ret
    }

    def append(l: SCycleList[T]) = {
      while ( {
        l.count != 0
      })
        push_back(l.pop_front.get)
      this
    }


    def q_sort_helper(comparator: Comparator[T]): SCycleList[T] = {
      if (is_empty || count == 1) return this
      val pivot = head.data
      val less = new Predicate[T] {
        override def test(t: T) = comparator.compare(t, pivot) < 0}
      val eq = new Predicate[T] {
        override def test(t: T) = 0 == comparator.compare(t, pivot)}
      val great = new Predicate[T] {
        override def test(t: T) = comparator.compare(t, pivot) > 0}
      val ret = filter(less).q_sort_helper(comparator)
      .append(filter(eq))
      .append(filter(great).q_sort_helper(comparator))
      ret
    }

    def q_sort(comparator: Comparator[T]): Unit = {
      if (is_empty || count == 1) return
      head = q_sort_helper(comparator).head
    }

}
