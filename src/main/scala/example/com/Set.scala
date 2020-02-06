package example.com

class Set[A](store: Node[A]) {
  def add(el: A): Set[A] = {
    if (!store.contains(el)) {
      new Set(store.insert(el).asInstanceOf[Node[A]])
    } else
      this
  }

  def toList(): List[A] = {
    store.toList
  }
}

object Set {
  def apply[A](data: A): Set[A] = new Set(Node(data))
}
