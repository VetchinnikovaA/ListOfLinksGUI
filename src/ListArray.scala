import java.util
import java.util.Collections
import java.util.function.Consumer
import java.util.Vector

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import java.util.function.Consumer
import java.util.{Comparator, Random}

import scala.runtime.Nothing$

class ListArray[T<% Ordered[T] : ClassTag] {

  private var head: Node[T] = null
  private var tail: Node[T] = null
  var count = 0
  head=tail

  def is_empty: Boolean = head == null

  def add(value: T): ListArray[T] = {
    if (is_empty) {
      var n = new Node[T]
      n.set(value)
      head = n
      tail = head
      head.setIndex(count)
      count+=1
    }
    else {
      tail.set(value)
      if(tail.getNext!=null) {
        tail=tail.getNext
        tail.setIndex(count)
        count+=1
      }
    }
    this
  }

  @throws[Exception]
  def insert(data: T, n: Int): ListArray[T] = { //вставка но номеру
    var nn=n
    var i = head
    while (true) {
      nn -= i.getCount
      if (nn < 0){
        i.setNumber(data, nn + i.getCount)
          if (tail.getNext != null) {
            tail=tail.getNext
            count+=1
            tail.setIndex(count)
          }
        return this
      }
      if (i eq tail) throw new Exception("Выход за пределы списка")
      i = i.getNext
    }
    this
  }

  @throws[Exception]
  def get(nn: Int): Option[T] = {
    if (nn == null) return null
    var n=nn
    var i = head
    while (true) {
      n -= i.getCount
      if (n < 0)
        return Option(i.getData(n + i.getCount))
      if (i eq tail)
        throw new Exception("Выход за пределы списка")
      i = i.getNext
    }
    Option(i.getData(n + i.getCount))
  }

  @throws[Exception]
  def remove(nn: Int): Option[T]  = {
    if (nn == null) return null
    var n=nn
    var i = head
    while (true) {
      n -= i.getCount
      if (n < 0) {
        val t:T=i.getData(n + i.getCount)
        if (i.remove(n + i.getCount)) {
          if (tail.getNext != null) {
            tail = tail.getNext
          }
          if(tail.getIndex!=count-1)
            count-=1
          return Option(t)
        }
        else {
          tail = retail
          count-=1
          return Option(t)
        }
      }
      if (i eq tail)
        throw new Exception("Выход за пределы списка")
      i = i.getNext
    }
    Option(i.getData(n))
  }

  def for_each(action: Consumer[T]): Unit = {
    var cur = head
    var c=0
    while ( c < count) {
      var i = 0
      while (i < cur.getCount) {
        action.accept(cur.getData(i))
        i += 1
      }
      cur = cur.getNext
      c+=1
    }
  }

  def print(): Unit = { //вывод
    var i = head
    var c=1
    while ( c <= count) {
      System.out.print(i.getIndex + ": ")
      i.show
      i = i.getNext
      c+=1
    }
  }

  def printConsole(): String = { //вывод
  var i = head
    var c=1
    var s: String=""
    while ( c <= count) {
      s+=i.getIndex + ": "
      s+= i.showConsole()
      i = i.getNext
      c+=1
    }
    s
  }

  def retail: Node[T] = {
    var i = head
    while (true) {
      if (i.getNext eq tail) {
        i.setNext(null)
        return i
      }
      i = i.getNext
    }
    i
  }

  def toArrayBuffer: ArrayBuffer[T] = {
    val allValues = new ArrayBuffer[T]
    var node = head
    while (node != null) {
      for (i <- 0 until node.getCount)
        allValues+=node.getData(i)
      node = node.getNext
    }
    allValues
  }

  def fromArrayBuffer(v: ArrayBuffer[T]): Node[T] = {
    var node = head
    var k=0
    while (node != null) {
      for (i <- 0 until node.getCount) {
        node.setData(i, v(k))
        k+=1
      }
      node = node.getNext
    }
    head
  }

  def sort(lt: (T, T) => Boolean) : ListArray[T]=  {
    val asArr = this.toArrayBuffer
      asArr.sorted
    this.fromArrayBuffer(asArr)
    this
  }
  def sort(lt: Boolean) : ListArray[T]= {
    val asArr = this.toArrayBuffer
    asArr.sortWith(if(lt) _.compareTo(_)>0 else _.compareTo(_)<0)
    for(i<-0 until asArr.size)
      System.out.println(asArr(i))
    this.fromArrayBuffer(asArr)
    this
  }


}
