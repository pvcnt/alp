package fr.cnrs.liris.accio.lib.param

import fr.cnrs.liris.util.testing.UnitSpec

/**
 * Unit tests for [[ParamMap]].
 */
class ParamMapSpec extends UnitSpec {

  class FooClass(override val uid: String = "foo") extends Identifiable

  val foo = new FooClass
  val bar = new FooClass("bar")
  val p1 = new Param[Int](foo, "p1", "", None)
  val p2 = new Param[Int](foo, "p2", "", None)
  val p3 = new Param[Int](foo, "p3", "", Some(3))
  val p1bis = new Param[Int](bar, "p1", "", None)
  val p4 = new Param[Int](bar, "p4", "", None)

  "ParamMap" should "create an empty instance" in {
    val map = ParamMap.empty
    map.size shouldBe 0
    map.isEmpty shouldBe true
    map.nonEmpty shouldBe false
    map.toSeq should have size 0
  }

  it should "create an instance from ParamPair's" in {
    val map = ParamMap(p1 := 1, p2 := 2, p3 := 3)
    map.size shouldBe 3
    map.isEmpty shouldBe false
    map.nonEmpty shouldBe true
    map.toSeq should contain theSameElementsAs Seq(p1 := 1, p2 := 2, p3 := 3)
  }

  it should "check if a param has been explicitly set" in {
    val map = ParamMap(p1 := 1)
    map.contains(p1) shouldBe true
    map.contains(p2) shouldBe false
    map.contains(p3) shouldBe false // Even if a default value exists
  }

  it should "get the optional explicit value" in {
    val map = ParamMap(p1 := 1)
    map.get(p1) shouldBe Some(1)
    map.get(p2) shouldBe None
    map.get(p3) shouldBe None // Even if a default value exists
  }

  it should "get the optional explicit or default value" in {
    val map = ParamMap(p1 := 1)
    map.getOrElse(p1) shouldBe Some(1)
    map.getOrElse(p2) shouldBe None
    map.getOrElse(p3) shouldBe Some(3)
  }

  it should "return the explicit, default or user-provided value" in {
    val map = ParamMap(p1 := 1)
    map.getOrElse(p1, 11) shouldBe 1 // Explicit value takes precedence
    map.getOrElse(p2, 22) shouldBe 22
    map.getOrElse(p3, 33) shouldBe 3 // Default value takes precedence
  }

  it should "return the value or default value" in {
    val map = ParamMap(p1 := 1)
    map(p1) shouldBe 1
    an[NoSuchElementException] shouldBe thrownBy {
      map(p2)
    }
    map(p3) shouldBe 3
  }

  it should "merge with another instance" in {
    val map1 = ParamMap(p1 := 1, p2 := 2)
    val map2 = ParamMap(p1 := 11, p3 := 3)
    val map = map1 ++ map2

    // Original maps are unchanged.
    map1.get(p1) shouldBe Some(1)
    map1.get(p2) shouldBe Some(2)
    map1.size shouldBe 2
    map2.get(p1) shouldBe Some(11)
    map2.get(p3) shouldBe Some(3)
    map2.size shouldBe 2

    // Result map is correct, map2 takes precedence.
    map.get(p1) shouldBe Some(11)
    map.get(p2) shouldBe Some(2)
    map.get(p3) shouldBe Some(3)
    map.size shouldBe 3
  }

  it should "set new parameters by values" in {
    val map1 = ParamMap(p1 := 1)
    val map2 = map1.set(p1, 11)
    val map3 = map1.set(p2, 2)

    // Original map isunchanged.
    map1.get(p1) shouldBe Some(1)
    map1.size shouldBe 1

    // Result maps are correct.
    map2.get(p1) shouldBe Some(11)
    map2.size shouldBe 1
    map3.get(p1) shouldBe Some(1)
    map3.get(p2) shouldBe Some(2)
    map3.size shouldBe 2
  }

  it should "set new parameters by ParamPair's" in {
    val map1 = ParamMap(p1 := 1)
    val map2 = map1.set(p1 := 11)
    val map3 = map1.set(p1 := 11, p2 := 2)

    // Original map isunchanged.
    map1.get(p1) shouldBe Some(1)
    map1.size shouldBe 1

    // Result maps are correct.
    map2.get(p1) shouldBe Some(11)
    map2.size shouldBe 1
    map3.get(p1) shouldBe Some(11)
    map3.get(p2) shouldBe Some(2)
    map3.size shouldBe 2
  }

  it should "filter by parent" in {
    val map = ParamMap(p1 := 1, p2 := 2, p4 := 4)
    val fooMap = map.filter(foo)
    val barMap = map.filter(bar)

    // Original maps are unchanged.
    map.contains(p1) shouldBe true
    map.contains(p2) shouldBe true
    map.contains(p4) shouldBe true
    map.size shouldBe 3

    // Result maps are correct.
    fooMap.get(p1) shouldBe Some(1)
    fooMap.get(p2) shouldBe Some(2)
    fooMap.size shouldBe 2
    barMap.get(p4) shouldBe Some(4)
    barMap.size shouldBe 1
  }
}