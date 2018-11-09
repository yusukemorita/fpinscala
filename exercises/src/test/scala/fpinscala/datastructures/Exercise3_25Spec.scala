package fpinscala.datastructures

import org.scalatest._

class Exercise3_25Spec extends FlatSpec {

  val leafA = Leaf("a")
  val leafB = Leaf("b")
  val branch = Branch(leafA, leafB)
  val branchWithBranch = Branch(leafA, branch)

  val treesWithSizes = Seq(
    (leafA, 1),
    (branch, 3),
    (branchWithBranch, 5)
  )

  for((tree, size) <- treesWithSizes) {
    s"size of ${tree.toString}" should s"return $size" in {
      assert(Tree.size(tree) == size)
    }
  }
}
