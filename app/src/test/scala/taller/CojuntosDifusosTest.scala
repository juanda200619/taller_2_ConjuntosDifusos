package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ConjuntosDifusosTest extends AnyFunSuite {
  val objConjuntosDifusos = new ConjuntosDifusos()

    // Test funcion pertenece

  test("Pertenece 10 en grande(2,2)") {
    assert(objConjuntosDifusos.pertenece(10, objConjuntosDifusos.grande(2, 2)) == 0.6)
  }

  test("Pertenece 10 en complemento(grande(2,2))") {
    assert(objConjuntosDifusos.pertenece(10, objConjuntosDifusos.complemento(objConjuntosDifusos.grande(2, 2))) ==  0.4)
  }


  // Test funcion inclusion

  test("Inclusion grande(2,2) en grande(9,18)") {
    assert(!objConjuntosDifusos.inclusion(objConjuntosDifusos.grande(2, 2), objConjuntosDifusos.grande(9, 18)))
  }

  test("Inclusion grande(2,2) en complemento(grande(2,2))") {
    assert(!objConjuntosDifusos.inclusion(objConjuntosDifusos.grande(2, 2), objConjuntosDifusos.complemento(objConjuntosDifusos.grande(2, 2))))
  }
}