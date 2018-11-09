package fpinscala.datastructures

import org.scalatest._

class Exercise3_27Spec extends FlatSpec {

  val leafA = Leaf(1)
  val leafB = Leaf(2)
  val depth2 = Branch(leafA, leafB)
  val depth3 = Branch(Leaf(3), depth2)
  val depth4 = Branch(depth3, leafA)

  val treesWithDepths = Seq(
    (leafA, 1),
    (depth2, 2),
    (depth3, 3),
    (depth4, 4)
  )

  for((tree, depth) <- treesWithDepths) {
    s"depth of ${tree.toString}" should s"return $depth" in {
      assert(Tree.depth(tree) == depth)
    }
  }
}
