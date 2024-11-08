package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.checkerframework.checker.units.qual.t

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

  test("Pertenece 10 en union(grande(2,2), grande(9,18))") {
    assert(objConjuntosDifusos.pertenece(10, objConjuntosDifusos.union(objConjuntosDifusos.grande(2, 2), objConjuntosDifusos.grande(9, 18))) == 0.6)
  }

  test("Pertenece 10 en interseccion(grande(2,2), grande(9,18))") {
    assert(objConjuntosDifusos.pertenece(10, objConjuntosDifusos.interseccion(objConjuntosDifusos.grande(2, 2), objConjuntosDifusos.grande(9, 18))) == 0.0)
  }

  test("Pertenece 10 en grande(1,3)") {
    assert(objConjuntosDifusos.pertenece(10, objConjuntosDifusos.grande(1, 3)) == 0.7)
  }

  // Test funcion inclusion

  test("Inclusion grande(2,2) en grande(9,18)") {
    assert(!objConjuntosDifusos.inclusion(objConjuntosDifusos.grande(2, 2), objConjuntosDifusos.grande(9, 18)))
  }

  test("Inclusion grande(2,2) en complemento(grande(2,2))") {
    assert(!objConjuntosDifusos.inclusion(objConjuntosDifusos.grande(2, 2), objConjuntosDifusos.complemento(objConjuntosDifusos.grande(2, 2))))
  }

  test("Inclusion grande(1,3) en grande(3,6)") {
    assert(!objConjuntosDifusos.inclusion(objConjuntosDifusos.grande(1, 3), objConjuntosDifusos.grande(3, 6)))
  }

  test("Inclusion grande(2,4) en grande(3,5)") {
    assert(!objConjuntosDifusos.inclusion(objConjuntosDifusos.grande(2, 4), objConjuntosDifusos.grande(3, 5)))
  }

  test("Inclusion grande(5,10) en grande(7,12)") {
    assert(!objConjuntosDifusos.inclusion(objConjuntosDifusos.grande(5, 10), objConjuntosDifusos.grande(7, 12)))
  }

  // Tests funcion union
  test("Union grande(2,2) y grande(9,18)") {
    val union = objConjuntosDifusos.union(objConjuntosDifusos.grande(2, 2), objConjuntosDifusos.grande(9, 18))
    assert(union(10) == 0.6)
  }

  test("Union grande(1,3) y grande(3,6)"){
    val union = objConjuntosDifusos.union(objConjuntosDifusos.grande(1, 3), objConjuntosDifusos.grande(3, 6))
    assert(union(10) == 0.7)
  }

  test("Union grande(0,5) y grande(5,10)") {
    val union = objConjuntosDifusos.union(objConjuntosDifusos.grande(0, 5), objConjuntosDifusos.grande(5, 10))
    assert(union(5) == 1.0)
  }

  test("Union grande(1,25) y grande(1,15)") {
    val union = objConjuntosDifusos.union(objConjuntosDifusos.grande(1, 25), objConjuntosDifusos.grande(1, 15))
    assert(union(26) == 0.5)
  }

  test("Union grande(2,4) y grande(3,5)") {
    val union = objConjuntosDifusos.union(objConjuntosDifusos.grande(2, 4), objConjuntosDifusos.grande(3, 5))
    assert(union(5) == 0.2)
  }

  // Tests funcion intersección
  test("Interseccion grande(2,2) y grande(9,18)") {
    val interseccion = objConjuntosDifusos.interseccion(objConjuntosDifusos.grande(2, 2), objConjuntosDifusos.grande(9, 18))
    assert(interseccion(10) == 0.0) // El mínimo debe ser 0.0 ya que grande(9, 18) tiene 0.0
  }

  test("Intersección grande(1, 5) y grande(3,5)") {
    val interseccion = objConjuntosDifusos.interseccion(objConjuntosDifusos.grande(1, 5), objConjuntosDifusos.grande(3, 5))
    assert(interseccion(6) == 0.1) // El mínimo correcto
  }

  test("Intersección grande(1,25) y grande(1,15)") {
      val interseccion = objConjuntosDifusos.interseccion(objConjuntosDifusos.grande(1, 25), objConjuntosDifusos.grande(1,15))
      assert(interseccion(26) == 0.3) // El mínimo correcto
  }

  test("Intersección grande(2,4) y grande(3,5)") {
    val interseccion = objConjuntosDifusos.interseccion(objConjuntosDifusos.grande(2, 4), objConjuntosDifusos.grande(3, 5))
    assert(interseccion(5) == 0.0) // El mínimo correcto
  }

  test("Intersección grande(5,10) y grande(7,12)") {
    val interseccion = objConjuntosDifusos.interseccion(objConjuntosDifusos.grande(5, 10), objConjuntosDifusos.grande(7, 12))
    assert(interseccion(10) == 0.0) // El mínimo correcto
  }

  // Tests funcion igualdad
  test("Igualdad grande(2,2) y grande(2,2)") {
    assert(objConjuntosDifusos.igualdad(objConjuntosDifusos.grande(2, 2), objConjuntosDifusos.grande(2, 2))) // Deben ser iguales por tanto retorna true
  }

  test("Igualdad grande(1,3) y grande(3,6)") {
    assert(!objConjuntosDifusos.igualdad(objConjuntosDifusos.grande(1, 3), objConjuntosDifusos.grande(3, 6))) // No deben ser iguales por tanto retorna false
  
  }

  test("Igualdad grande(1,25) y grande(1,15)") {
    assert(!objConjuntosDifusos.igualdad(objConjuntosDifusos.grande(1, 25), objConjuntosDifusos.grande(1, 15))) // No deben ser iguales por tanto retorna false
  }

  test("Igualdad grande(2,4) y grande(3,5)") {
    assert(!objConjuntosDifusos.igualdad(objConjuntosDifusos.grande(2, 4), objConjuntosDifusos.grande(3, 5))) // No deben ser iguales por tanto retorna false
  }

  test("Igualdad grande(5,10) y grande(7,12)") {
    assert(!objConjuntosDifusos.igualdad(objConjuntosDifusos.grande(5, 10), objConjuntosDifusos.grande(7, 12))) // No deben ser iguales por tanto retorna false
  }

}