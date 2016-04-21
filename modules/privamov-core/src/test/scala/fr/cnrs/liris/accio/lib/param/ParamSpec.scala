package fr.cnrs.liris.accio.lib.param

import fr.cnrs.liris.util.testing.UnitSpec

import scala.reflect.runtime.universe._

/**
 * Unit tests for [[Param]].
 */
class ParamSpec extends UnitSpec {
  class FooClass(override val uid: String = "foo") extends Identifiable

  "Param" should "allow valid names" in {
    noException shouldBe thrownBy {
      Param.validateName("a")
      Param.validateName("a_01_Bc_ef")
    }
    noException shouldBe thrownBy {
      new Param[Int](new FooClass, "a", "", None)
      new Param[Int](new FooClass, "a_01_Bc_ef", "", None)
    }
  }

  it should "reject names not starting with a letter" in {
    an[IllegalArgumentException] shouldBe thrownBy {
      Param.validateName("0df")
    }
    an[IllegalArgumentException] shouldBe thrownBy {
      new Param[Int](new FooClass, "0df", "", None)
    }
    an[IllegalArgumentException] shouldBe thrownBy {
      Param.validateName("_df")
    }
    an[IllegalArgumentException] shouldBe thrownBy {
      new Param[Int](new FooClass, "_df", "", None)
    }
  }

  it should "reject names having non alpha-numeric characters" in {
    an[IllegalArgumentException] shouldBe thrownBy {
      Param.validateName("a d f")
    }
    an[IllegalArgumentException] shouldBe thrownBy {
      new Param[Int](new FooClass, "a d f", "", None)
    }
  }

  it should "infer the TypeTag of values" in {
    new Param[Int](new FooClass, "foo", "", None).valueTypeTag.tpe =:= typeOf[Int] shouldBe true
    new Param[String](new FooClass, "foo", "", None).valueTypeTag.tpe =:= typeOf[String] shouldBe true
  }

  it should "produce ParamPair's" in {
    val param = new Param[Int](new FooClass, "foo", "", None)
    val paramPair = param := 2
    paramPair.param shouldBe param
    paramPair.value shouldBe 2
  }

  it should "produce ParamDomain's" in {
    val param = new Param[Int](new FooClass, "foo", "", None)
    val domain = Domain.value(2)
    val paramDomain = param ~= domain
    paramDomain.param shouldBe param
    paramDomain.domain shouldBe domain
  }

  it should "compare to other Param's well" in {
    val p1 = new Param[Int](new FooClass("foo"), "p1", "", None)
    val p2 = new Param[Int](new FooClass("bar"), "p1", "", None)
    val p3 = new Param[Int](new FooClass("foo"), "p2", "", None)
    val p4 = new Param[Int](new FooClass("foo"), "p1", "With a description", None)
    val p5 = new Param[Int](new FooClass("foo"), "p1", "", Some(1))
    p1 should not be p2
    p1.hashCode should not be p2.hashCode
    p1 should not be p3
    p1.hashCode should not be p3.hashCode
    p1 shouldBe p4
    p1.hashCode shouldBe p4.hashCode
    p1 shouldBe p5
    p1.hashCode shouldBe p5.hashCode
  }
}