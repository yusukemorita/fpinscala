package fpinscala.errorhandling

import org.scalatest._

class Exercise4_1Spec extends FlatSpec {

  // map
  "Some(1).map(_ + 1)" should s"return Some(2)" in {
    assert(Some(1).map(_ + 1) === Some(2))
  }

  "None.map(_ + 1)" should s"None" in {
    assert(None.map((a: Int) => a + 1) === None)
  }

  // getOrElse
  "Some(1).getOrElse(0)" should s"return 1" in {
    assert(Some(1).getOrElse(0) === 1)
  }

  "None.getOrElse(0)" should s"0" in {
    assert(None.getOrElse(0) === 0)
  }

  // flatMap
  "Some(1).flatMap(a => Some(a + 1))" should s"return Some(2)" in {
    assert(Some(1).flatMap(a => Some(a + 1)) === Some(2))
  }

  "None.flatMap(a => Some(a + 1))" should s"return None" in {
    assert(None.flatMap((a: Int) => Some(a + 1)) === None)
  }

  // orElse
  "Some(1).orElse(Some(0))" should s"return Some(1)" in {
    assert(Some(1).orElse(Some(0)) === Some(1))
  }

  "None.orElse(Some(0))" should s"return Some(0)" in {
    assert(None.orElse(Some(0)) === Some(0))
  }

  // filter
  "Some(2).filter(_ > 1)" should s"return Some(1)" in {
    assert(Some(1).filter(_ > 0) === Some(1))
  }

  "Some(0).filter(_ > 1)" should s"return None" in {
    assert(Some(0).filter(_ > 0) === None)
  }
}
