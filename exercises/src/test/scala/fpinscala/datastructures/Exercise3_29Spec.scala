package fpinscala.datastructures

import org.scalatest._

class Exercise3_29Spec extends FlatSpec {

  val tree: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))

  "sum" should "return 6" in {
    val result = Tree.fold(tree)(a => a)((a: Int, b: Int) => a + b)
    assert(result == 6)
  }

  "maximum" should "return 3" in {
    val result = Tree.fold(tree)(a => a)((a: Int, b: Int) => a max b)
    assert(result == 3)
  }

  "depth" should "return 2" in {
    val result = Tree.fold(tree)(_ => 1)((a: Int, b: Int) => 1 + a max b)
    assert(result == 2)
  }

  "map(_ + 1)" should "return tree with one added to each element" in {
    val result = Tree.fold(tree)(a => Leaf(a + 1): Tree[Int])(Branch(_, _))
    assert(result == Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
  }

}
