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
        def comp(n : Int) : Double ={
            1 - c(n)
        }
        comp
    }

    def union(cd1: ConjDifuso , cd2: ConjDifuso) : Unit = {
    // Implementaci´on de la funci´on union, debe retornar un ConjDifuso
    }

    def interseccion (cd1: ConjDifuso , cd2: ConjDifuso) : Unit = {
    // Implementaci´on de la funci´on interseccion, debe retornar un ConjDifuso

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
    

    def igualdad(cd1: ConjDifuso , cd2: ConjDifuso) : Unit = {
    // Implementaci´on de la funci´on igualdad, debe retornar un ConjDifuso
    }

}