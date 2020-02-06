import example.com.Set
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

object SetPropertyTest extends Properties("example.com.Set") {
  implicit val arbitraryAVLTree: Arbitrary[Set[String]] = Arbitrary(for {tile <- Arbitrary.arbitrary[String]} yield Set(tile))

  property("Size of list = 1") = forAll { (el: String) =>
    Set[String](el).toList().size == 1
  }

  property("Size of list with five or less duplicate elements = 5") =
    forAll { (el1: Double, el2: Double, el3: Double, el4: Double, el5: Double) =>
      Set[Double](el1).add(el2).add(el3).add(el4).add(el5).add(el1).add(el4).toList().size <= 5
    }

  property("Type value Set[Option[String]] is Option[String]") =
    forAll { (el1: Option[String]) =>
      Set[Option[String]](el1).toList().map(_.isInstanceOf[Option[String]]) == List(true)
    }
}
