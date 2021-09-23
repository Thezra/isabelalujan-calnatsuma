package co.s4n.calnat
import scala.io.StdIn

object Main extends App {
    def leerInt(prompt:String):Int = {
        val s = StdIn.readLine(prompt)
        s.toInt
    }

    def esCero(nat:Nat) = nat match {
        case Cero() => true
        case Suc(nat) => false
    }

    def esMayorIgual(nat1:Nat,nat2:Nat):Boolean = nat1 match {
        case Cero() => nat2 match {
            case Cero() => true
            case _      => false
        }
        case Suc(pnat) => nat2 match {
            case Cero() => true
            case Suc(snat) => esMayorIgual(pnat, snat)
        }
    }

    def conIntANat(i:Int):Nat = i match {
        case 0 => Cero()
        case i => Suc(conIntANat(i-1))
    }

    def imprimirNat(nat:Nat):String = println(nat.toString)

    def restaNat(nat1:Nat,nat2:Nat):Nat = {
        if (esMayorIgual(nat1,nat2) == true) {
            val lista1 = nat1.split("(").map(_.toList)
            val lista2 = nat1.split("(").map(_.toList)
            val resta = lista1.length-lista2.length
            conIntANat(resta)
        }
        else {
            println("No se puede hacer la operación")
        }
    }
    val primerEntero = leerInt("Leer primer entero: ")
    val segundoEntero = leerInt("Leer segundo entero: ")

    println(imprimirNat(restaNat(primerEntero,segundoEntero)))
}