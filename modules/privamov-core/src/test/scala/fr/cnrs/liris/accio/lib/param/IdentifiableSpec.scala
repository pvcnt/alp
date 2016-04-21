package fr.cnrs.liris.accio.lib.param

import fr.cnrs.liris.util.testing.UnitSpec

/**
 * Unit tests for [[Identifiable]].
 */
class IdentifiableSpec extends UnitSpec {
  "Identifiable" should "generate unique identifers" in {
    Seq.fill(100)(Identifiable.uniqid("foo")).distinct should have size 100
  }

  it should "generate unique identifiers including the prefix" in {
    Identifiable.uniqid("foo") should startWith("foo")
    Identifiable.uniqid("bar") should startWith("bar")
    Identifiable.uniqid("foobar") should startWith("foobar")
  }
}
