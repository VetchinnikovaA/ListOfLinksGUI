import scala.reflect.ClassTag

class Node[T <% Ordered[T]: ClassTag] {
  val size=4
  private val data: Array[T] = new Array[T](size)
  private var next: Node[T] = null
  private var count=0
  private var index:Int=_

  def setNext(_next: Node[T]): Node[T] = {
    next = _next
    this
  }

  def getNext: Node[T] = next

  def getCount: Int = count

  def getIndex: Int = index

  def setIndex(_index: Int): Node[T] = {
    index=_index
    this
  }

  def getData(i: Int): T = data(i)

  def setData(i: Int, d: T): Node[T] = {
    data(i) = d
    this
  }

  def set(value: T): Node[T] = {
    if (count < size) {
      data(count) = value
      count += 1
    }
    else
      expend(value)
    this
  }

  def expend(value: T): Node[T] = {
    val newNode = new Node[T]
    var i = size / 2
    var j = 0
    while (i < size) {
      newNode.data(j) = data(i)
      count -= 1
      newNode.count += 1

      j += 1
      i += 1
    }
    newNode.data(size / 2) = value
    newNode.count += 1
    newNode.next = next
    if (next == null)
    this.next = newNode
    this
  }

  def setNumber(d: T, n: Int): Node[T] = {
    if (n < count) {
      if (count < size) {
        for (i <- count until n by -1)
          data(i) = data(i - 1)
        data(n) = d
        count += 1
        this
      }
      else {
        if (next == null) {
          next = new Node[T]
          next.setIndex(index + 1)
        }
        next.setNumber(data(size - 1), 0)
        for (i <- count - 1 until n by -1)
          data(i) = data(i - 1)
        data(n) = d
        this
      }
    }
    else {
      this.set(d)
      this
    }
  }

  def show(): Unit = {
    for (i <- 0 until count) {
      System.out.print(data(i)+" ")
    }
    System.out.println()
  }

  def showConsole(): String = {
    var s: String=""
    for (i <- 0 until count) {
      s+=data(i)+" "
    }
    s+="\n"
    s
  }

  def remove(n: Int): Boolean = {
    for (i <- n until count - 1)
      data(i) = data(i + 1)
    count -= 1
    if (count == 0) {
      if (next == null)
        return false
      this.reData(next)
      this.next = next.next
      var i = this
      while (true) {
        if (i.getNext != null)
          i.getNext.setIndex(i.getNext.getIndex - 1)
        else
          return true

        i = i.getNext
      }
    }
    true
  }

  def reData(node: Node[T]): Node[T] = {
    count = node.count
    for (i <- 0 until size) {
      data(i) = node.data(i)
    }
    this
  }
}