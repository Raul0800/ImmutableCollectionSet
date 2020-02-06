package example.com

object Main {
  def main(args: Array[String]): Unit = {
    //example Set

    //initialize
    val set1: Set[Int] = Set(1)
    val set2: Set[String] = Set("hi!")

    //get list from Set
    println(set1.toList())
    println(set2.toList())

    //add elements and print list of set elements
    println(set1.add(2).add(3).add(2).toList())
    println(set2.add("hello!").add("hello!").add("hey!").add("hi!").toList())
  }
}
