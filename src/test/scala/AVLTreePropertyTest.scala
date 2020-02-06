import example.com.Node
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

object AVLTreePropertyTest extends Properties("example.com.AVLTree") {
  implicit val arbitraryAVLTree: Arbitrary[Node[String]] = Arbitrary(for {tile <- Arbitrary.arbitrary[String]} yield Node(tile))

  property("Hight of tree should be <= 1") = forAll { (el: String) =>
    Node[String](el).depth <= 0
  }

  property("Hight of tree should be smaller then 1.45 * log_2(n + 2)") = forAll { (el1: Int, el2: Int, el3: Int) =>
    Node[Int](el1).insert(el2).insert(el3).depth <= 1.45 * scala.math.log(5)
  }

  property("Two eq values get one Node with depth <= 1") = forAll { (el1: String) =>
    Node[String](el1).insert(el1).depth <= 1
  }

  property("If we add 5 elements then size of list will be <= 5") =
    forAll { (el1: Double, el2: Double, el3: Double, el4: Double, el5: Double) =>
      Node[Double](el1).insert(el2).insert(el3).insert(el4).insert(el5).toList.size <= 5
    }
}

