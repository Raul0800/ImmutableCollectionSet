import example.com.Set
import org.scalatest.{FlatSpec, _}

class SetUnitTest extends FlatSpec with Matchers {
  "Add: 1, 2, 3, 4" should "list.size = 4, type: Int" in {
    val set = Set[Int](1).add(2).add(3).add(4)
    set.toList.size should be(4)
    set.toList().map(_.isInstanceOf[Int]) should be(List(true, true, true, true))
  }

  "Add: 1, -1, 1, -1, 1, -1, 2, -2, 2, -2, 2, -2" should "list.size = 4, type: Double" in {
    val set = Set[Double](1)
      .add(-1)
      .add(1)
      .add(-1)
      .add(1)
      .add(-1)
      .add(2)
      .add(-2)
      .add(2)
      .add(-2)
      .add(2)
      .add(-2)
    set.toList.size should be(4)
    set.toList().map(_.isInstanceOf[Double]) should be(List(true, true, true, true))
  }
}