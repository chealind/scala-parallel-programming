package scalashop

import org.junit._

class BlurSuite {

  @Test def `testSimpleBoxBlur`(): Unit = {
    val radius = 1
    val x = 1
    val y = 1

    val img: Img = new Img(3, 3, List(
      2,2,2,
      2,1,2,
      2,2,2).toArray)

    val result = boxBlurKernel(img, x, y, radius)
    val expected: RGBA = 1

    assert(expected == result, s"expected $expected but was $result")

    val img2: Img = new Img(3, 3, List(
      2,2,2,
      2,1,4,
      4,4,4).toArray)

    val result2 = boxBlurKernel(img2, x, y, radius)
    val expected2: RGBA = 2

    assert(expected2 == result2, s"expected $expected2 but was $result2")
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
