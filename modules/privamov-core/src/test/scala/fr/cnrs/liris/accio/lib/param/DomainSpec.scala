package fr.cnrs.liris.accio.lib.param

import fr.cnrs.liris.util.testing.UnitSpec

class DomainSpec extends UnitSpec {
  "Domain" should "return its values" in {
    val domain = Domain(Seq(1, 2, 3, 4, 5))
    domain.values shouldBe Seq(1, 2, 3, 4, 5)
  }

  it should "pick a random element" in {
    val domain = Domain(Seq(1, 2, 3, 4, 5))
    for (i <- 0 until 10) {
      Set(1, 2, 3, 4, 5).contains(domain.random()) shouldBe true
    }
  }

  it should "tell whether it is a singleton" in {
    val domain = Domain(Seq(1, 2, 3, 4, 5))
    domain.isSingleton shouldBe false

    val domain2 = Domain(Seq(1))
    domain2.isSingleton shouldBe true
  }

  it should "reject an empty domain" in {
    an[IllegalArgumentException] shouldBe thrownBy {
      Domain(Seq.empty)
    }
  }

  it should "reject an invalid contraction" in {
    an[IllegalArgumentException] shouldBe thrownBy {
      Domain(Seq(1, 2, 3), contraction = 0)
    }
    an[IllegalArgumentException] shouldBe thrownBy {
      Domain(Seq(1, 2, 3), contraction = 1.1)
    }
  }

  it should "pick a neighbor element with no contraction" in {
    val domain = Domain(Seq(1, 2, 3, 4, 5), contraction = 1)
    Seq.fill(50)(domain.neighbor(1)).toSet shouldBe Set(2, 3, 4, 5)
    Seq.fill(50)(domain.neighbor(2)).toSet shouldBe Set(1, 3, 4, 5)
    Seq.fill(50)(domain.neighbor(3)).toSet shouldBe Set(1, 2, 4, 5)
    Seq.fill(50)(domain.neighbor(4)).toSet shouldBe Set(1, 2, 3, 5)
    Seq.fill(50)(domain.neighbor(5)).toSet shouldBe Set(1, 2, 3, 4)
  }

  it should "pick a neighbor element from an odd-sized domain" in {
    val domain = Domain(Seq(1, 2, 3, 4, 5), contraction = .5)
    Seq.fill(20)(domain.neighbor(1)).toSet shouldBe Set(2, 3)
    Seq.fill(20)(domain.neighbor(2)).toSet shouldBe Set(1, 3)
    Seq.fill(20)(domain.neighbor(3)).toSet shouldBe Set(2, 4)
    Seq.fill(20)(domain.neighbor(4)).toSet shouldBe Set(3, 5)
    Seq.fill(20)(domain.neighbor(5)).toSet shouldBe Set(3, 4)
  }

  it should "pick a neighbor element from an even-sized domain" in {
    val domain = Domain(Seq(1, 2, 3, 4, 5, 6), contraction = .5)
    Seq.fill(20)(domain.neighbor(1)).toSet shouldBe Set(2, 3)
    Seq.fill(20)(domain.neighbor(2)).toSet shouldBe Set(1, 3)
    Seq.fill(20)(domain.neighbor(3)).toSet shouldBe Set(2, 4)
    Seq.fill(20)(domain.neighbor(4)).toSet shouldBe Set(3, 5)
    Seq.fill(20)(domain.neighbor(5)).toSet shouldBe Set(4, 6)
    Seq.fill(20)(domain.neighbor(6)).toSet shouldBe Set(4, 5)
  }
}
