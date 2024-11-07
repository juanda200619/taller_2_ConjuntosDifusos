package taller
import scala.annotation.tailrec

class ConjuntosDifusos {

    type ConjDifuso = Int => Double
    
    def pertenece(elem: Int , s: ConjDifuso) : Double = {
        s (elem)
    }

    def grande(d: Int , e: Int): ConjDifuso = {
        def mma(n: Int): Double = {

            math.floor((math.pow((n.toDouble/(n+d).toDouble), e))*10) / 10
        }
        mma
    }

    def complemento(c: ConjDifuso) : ConjDifuso = {
        def comp(n : Int) : Double = {
            1 - c(n)
        }
        comp
    }


    def union(cd1: ConjDifuso , cd2: ConjDifuso) : ConjDifuso = {
        // Función interna que calcula la unión de dos conjuntos difusos para un elemento dado
        def unionF(n: Int): Double = {
            // La pertenencia del elemento n en la unión es el máximo grado de sus pertenencias en los dos conjuntos
            math.max(cd1(n), cd2(n))
        }
        // Retornamos la función interna como el resultado de la unión
        unionF
    }

 
    def interseccion (cd1: ConjDifuso , cd2: ConjDifuso) : ConjDifuso = {
        // Función interna que calcula la intersección de dos conjuntos difusos para un elemento dado
        def interseccionF(n: Int): Double = {
            // La pertenencia del elemento n en la intersección es el mínimo grado de sus pertenencias en los dos conjuntos
            math.min(cd1(n), cd2(n))
        }
        // Retornamos la función interna que proporciona el nuevo conjunto difuso correspondiente a la intersección
        interseccionF
    }

    def inclusion (cd1: ConjDifuso , cd2: ConjDifuso) : Boolean = {

        @tailrec
        def aux(n: Int): Boolean = {
            if (n > 1000) true // Si llegamos al final del intervalo, es porque todos los elementos cumplen
            else if (cd1(n) > cd2(n)) false // Si algún elemento no cumple, devolvemos false
            else aux(n + 1) // Seguimos con el siguiente elemento
        }

        aux(0) // Comenzamos desde el elemento 0
    }
    

    def igualdad(cd1: ConjDifuso, cd2: ConjDifuso): Boolean = {
        inclusion(cd1, cd2) && inclusion(cd2, cd1) // Si ambos conjuntos se incluyen mutuamente, son iguales
    }


    // Función para generar una cadena con la pertenencia de cada elemento de un rango en un conjunto difuso
    def imprimirConjunto(conjunto: ConjDifuso, rango: Range): String = {
        rango.map(n => s"Elemento $n: ${pertenece(n, conjunto)}").mkString("\n")
    }
}