import example.com.Node
import org.scalatest.{FlatSpec, _}

class AVLTreeUnitTest extends FlatSpec with Matchers {
  "Add: 1, 2, 3, 4" should "depth = 2, list.size = 4, contains: 2 => true, contains: 6 => false" in {
    val tree = Node[Int](1).insert(2).insert(3).insert(4)
    tree.depth should be(2)
    tree.toList.size should be(4)
    tree.contains(2) should be(true)
    tree.contains(6) should be(false)
  }

  "Add: 1, -1, 2, -2, 3, -3, 4, -4, 5, -5, 6, -6" should "depth = 2, list.size = 12, contains: 2 => true, contains: 7 => false" in {
    val tree = Node[Int](1)
      .insert(-1)
      .insert(2)
      .insert(-2)
      .insert(3)
      .insert(-3)
      .insert(4)
      .insert(-4)
      .insert(5)
      .insert(-5)
      .insert(6)
      .insert(-6)
    tree.depth should be(3)
    tree.toList.size should be(12)
    tree.contains(2) should be(true)
    tree.contains(7) should be(false)
  }
}