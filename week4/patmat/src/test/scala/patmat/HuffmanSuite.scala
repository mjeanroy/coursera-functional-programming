package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
      assert(weight(Leaf('b', 3)) === 3)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
      assert(chars(t1) === List('a', 'b'))
      assert(chars(Leaf('b', 3)) === List('b'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"hello, world\")") {
    assert(times("hello, world".toList) === List(('h', 1), ('e', 1), ('l', 3), ('o', 2), (',', 1), (' ', 1), ('w', 1), ('r', 1), ('d', 1)))
    assert(times("some text".toList) === List(('s', 1), ('o', 1), ('m', 1), ('e', 2), (' ', 1), ('t', 2), ('x', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton") {
    assert(singleton(List(Leaf('e', 1))) == true)
    assert(singleton(List(Leaf('e', 1), Leaf('a', 1))) == false)
    assert(singleton(Nil) == false)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
    assert(until(singleton, combine)(leaflist) == List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)))
  }

  test("create optimal tree") {
    val lst = "The Huffman encoding of this message should be three hundred and fifty-two bits long".toList
    val tree: CodeTree = createCodeTree(lst)
    val encodedValue = encode(tree)(lst)
    assert(encodedValue.length == 352)
  }

  test("decode sample") {
    val leafA = new Leaf('A', 8)
    val leafB = new Leaf('B', 3)
    val leafC = new Leaf('C', 1)
    val leafD = new Leaf('D', 1)
    val leafE = new Leaf('E', 1)
    val leafF = new Leaf('F', 1)
    val leafG = new Leaf('G', 1)
    val leafH = new Leaf('H', 1)
    val nodeCD = new Fork(leafC, leafD, "CD".toList, 2)
    val nodeEF = new Fork(leafE, leafF, "EF".toList, 2)
    val nodeGH = new Fork(leafG, leafH, "GH".toList, 2)
    val nodeEFGH = new Fork(nodeEF, nodeGH, "EFGH".toList, 4)
    val nodeBCD = new Fork(leafB, nodeCD, "BCD".toList, 5)
    val nodeBCDEFGH = new Fork(nodeBCD, nodeEFGH, "BCDEFGH".toList, 9)
    val nodeABCDEFGH = new Fork(leafA, nodeBCDEFGH, "ABCDEFGH".toList, 17)

    val decodedValue = decode(nodeABCDEFGH, List(1, 0, 0, 0, 1, 0, 1, 0))
    assert(decodedValue == "BAC".toList)
  }

  test("decode decoded secret") {
    assert(decodedSecret === "huffmanestcool".toList)
  }

  test("encode sample") {
    val leafA = new Leaf('A', 8)
    val leafB = new Leaf('B', 3)
    val leafC = new Leaf('C', 1)
    val leafD = new Leaf('D', 1)
    val leafE = new Leaf('E', 1)
    val leafF = new Leaf('F', 1)
    val leafG = new Leaf('G', 1)
    val leafH = new Leaf('H', 1)
    val nodeCD = new Fork(leafC, leafD, "CD".toList, 2)
    val nodeEF = new Fork(leafE, leafF, "EF".toList, 2)
    val nodeGH = new Fork(leafG, leafH, "GH".toList, 2)
    val nodeEFGH = new Fork(nodeEF, nodeGH, "EFGH".toList, 4)
    val nodeBCD = new Fork(leafB, nodeCD, "BCD".toList, 5)
    val nodeBCDEFGH = new Fork(nodeBCD, nodeEFGH, "BCDEFGH".toList, 9)
    val nodeABCDEFGH = new Fork(leafA, nodeBCDEFGH, "ABCDEFGH".toList, 17)

    val encodedValue = encode(nodeABCDEFGH)(List('D'))
    assert(encodedValue == List(1, 0, 1, 1))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits should return bits associated to char") {
    val A = ('A', List(1, 1, 1))
    val B = ('B', List(1, 1, 0))
    val C = ('C', List(1, 0, 0))
    val table = List(A, B, C)

    assert(codeBits(table)('A') == List(1, 1, 1))
    assert(codeBits(table)('B') == List(1, 1, 0))
    assert(codeBits(table)('C') == List(1, 0, 0))
  }

  test("merge two code table") {
    val A = ('A', List(1, 1, 1))
    val B = ('B', List(1, 1, 0))
    val C = ('C', List(1, 0, 0))
    val D = ('D', List(0, 0, 0))

    val table1: CodeTable = List(A, B)
    val table2: CodeTable = List(C, D)

    val mergedTable = mergeCodeTables(table1, table2)
    assert(mergedTable.size == 4)
    assert(mergedTable.contains(A))
    assert(mergedTable.contains(B))
    assert(mergedTable.contains(C))
    assert(mergedTable.contains(D))
  }

  test("convert") {
    val leafA = new Leaf('A', 8)
    val leafB = new Leaf('B', 3)
    val leafC = new Leaf('C', 1)
    val leafD = new Leaf('D', 1)
    val leafE = new Leaf('E', 1)
    val leafF = new Leaf('F', 1)
    val leafG = new Leaf('G', 1)
    val leafH = new Leaf('H', 1)
    val nodeCD = new Fork(leafC, leafD, "CD".toList, 2)
    val nodeEF = new Fork(leafE, leafF, "EF".toList, 2)
    val nodeGH = new Fork(leafG, leafH, "GH".toList, 2)
    val nodeEFGH = new Fork(nodeEF, nodeGH, "EFGH".toList, 4)
    val nodeBCD = new Fork(leafB, nodeCD, "BCD".toList, 5)
    val nodeBCDEFGH = new Fork(nodeBCD, nodeEFGH, "BCDEFGH".toList, 9)
    val nodeABCDEFGH = new Fork(leafA, nodeBCDEFGH, "ABCDEFGH".toList, 17)

    val table: CodeTable = convert(nodeABCDEFGH)
    val D = ('D', List(1, 0, 1, 1))
    assert(table.size == 8)
    assert(table contains D)
  }

  test("quick encode sample") {
    val leafA = new Leaf('A', 8)
    val leafB = new Leaf('B', 3)
    val leafC = new Leaf('C', 1)
    val leafD = new Leaf('D', 1)
    val leafE = new Leaf('E', 1)
    val leafF = new Leaf('F', 1)
    val leafG = new Leaf('G', 1)
    val leafH = new Leaf('H', 1)
    val nodeCD = new Fork(leafC, leafD, "CD".toList, 2)
    val nodeEF = new Fork(leafE, leafF, "EF".toList, 2)
    val nodeGH = new Fork(leafG, leafH, "GH".toList, 2)
    val nodeEFGH = new Fork(nodeEF, nodeGH, "EFGH".toList, 4)
    val nodeBCD = new Fork(leafB, nodeCD, "BCD".toList, 5)
    val nodeBCDEFGH = new Fork(nodeBCD, nodeEFGH, "BCDEFGH".toList, 9)
    val nodeABCDEFGH = new Fork(leafA, nodeBCDEFGH, "ABCDEFGH".toList, 17)

    val encodedValue = quickEncode(nodeABCDEFGH)(List('D'))
    assert(encodedValue == List(1, 0, 1, 1))
  }
}
