package example.com

sealed trait AVLTree[+A] {
  def balance: Int

  def depth: Int

  def contains[B >: A](value: B): Boolean = false

  def insert[B >: A](value: B): AVLTree[B] = Node(value, Leaf, Leaf)

  def rebalance[B >: A]: AVLTree[B] = this

  def leftRotation[B >: A]: Option[Node[B]] = None

  def rightRotation[B >: A]: Option[Node[B]] = None

  def doubleLeftRotation[B >: A]: Option[Node[B]] = None

  def doubleRightRotation[B >: A]: Option[Node[B]] = None

  def toList[B >: A]: List[B] = List.empty
}

case object Leaf extends AVLTree[Nothing] {
  override val balance: Int = 0

  override val depth: Int = -1
}

case class Node[+A](data: A, left: AVLTree[A] = Leaf, right: AVLTree[A] = Leaf) extends AVLTree[A] {
  override val balance: Int = right.depth - left.depth

  override val depth: Int = math.max(left.depth, right.depth) + 1

  override def contains[B >: A](value: B): Boolean = {
    value.hashCode() - data.hashCode() match {
      case 0 => true
      case x if x < 0 => left.contains(value)
      case _ => right.contains(value)
    }
  }

  override def insert[B >: A](value: B) = {
    value.hashCode() - data.hashCode() match {
      case x if x <= 0 => Node(data, left.insert(value), right)
        .rebalance
      case _ => Node(data, left, right.insert(value))
        .rebalance
    }
  }

  override def rebalance[B >: A] = {
    if (balance == -2) {
      if (left.balance == 1)
        doubleRightRotation.get
      else
        rightRotation.get
    } else if (balance == 2) {
      if (right.balance == -1)
        doubleLeftRotation.get
      else
        leftRotation.get
    } else {
      this
    }
  }

  override def leftRotation[B >: A] = {
    if (right != Leaf) {
      val r: Node[A] = right.asInstanceOf[Node[A]]
      Some(Node(r.data, Node(data, left, r.left), r.right))
    } else None
  }

  override def rightRotation[B >: A] = {
    if (left != Leaf) {
      val l: Node[A] = left.asInstanceOf[Node[A]]
      Some(Node(l.data, l.left, Node(data, l.right, right)))
    } else None
  }

  override def doubleLeftRotation[B >: A] = {
    if (right != Leaf) {
      val r: Node[A] = right.asInstanceOf[Node[A]]
      val rightRotated = r.rightRotation
      Some(Node(rightRotated.get.data, Node(data, left, rightRotated.get.left), rightRotated.get.right))
    } else None
  }

  override def doubleRightRotation[B >: A] = {
    if (left != Leaf) {
      val l: Node[A] = left.asInstanceOf[Node[A]]
      val leftRotated = l.leftRotation
      Some(Node(leftRotated.get.data, leftRotated.get.left, Node(data, leftRotated.get.right, right)))
    } else None
  }


  override def toList[B >: A]: List[B] = {
    var values = List.empty[A]

    def fold(node: Node[A]): List[A] = {
      if (node.left != Leaf) {
        fold(node.left.asInstanceOf[Node[A]])
      }
      values = values ++ List(node.data)
      if (node.right != Leaf) {
        fold(node.right.asInstanceOf[Node[A]])
      }
      values
    }

    fold(this)
  }
}