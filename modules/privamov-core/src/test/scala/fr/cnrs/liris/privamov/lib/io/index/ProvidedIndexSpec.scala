package fr.cnrs.liris.privamov.lib.io.index

import fr.cnrs.liris.util.testing.UnitSpec

/**
 * Unit tests for [[ProvidedIndex]].
 */
class ProvidedIndexSpec extends UnitSpec {
  "ProvidedIndex" should "have correct files" in {
    val files = Seq(
      IndexedFile("/foo/bar.csv"),
      IndexedFile("/bar/foo.csv", labels = Set("bar", "foo")),
      IndexedFile("/bar/bar.csv", labels = Set("bar")),
      IndexedFile("/foo/bar.txt", length = 30),
      IndexedFile("/foo/bar.txt", offset = 30))
    val index = new ProvidedIndex(files)
    index.labels should contain theSameElementsAs Set("foo", "bar")

    index.read(None) should contain theSameElementsAs files.toSet
    index.read(Some("foo")) should contain theSameElementsAs Set(files(1))
    index.read(Some("bar")) should contain theSameElementsAs Set(files(1), files(2))
    index.read(Some("boo")) should contain theSameElementsAs Set.empty
  }
}