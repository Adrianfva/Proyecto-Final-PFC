/**
  * Taller 3 - Programación Funcional
  * Autores: miguel angel cebalos 2259619, Cristhian leonardo Albarracin zapata 1968253 ,Nicolás Gutiérrez Ramírez 2259515
  * Profesor: Carlos A Delgado
  */
package taller4
//import common._

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import scala.util.Random
import common._
import taller4.TrieDefinitions._


object Taller4 {

  def compararAlgoritmos[T](funcion1: => T, funcion2: => T, numEjecuciones: Int = 1): (Double, Double, Double) = {

    def medirTiempo(funcion: => T): Double = {
      val tiempos = (1 to numEjecuciones).map { _ =>
        val inicio = System.nanoTime()
        try {
          funcion
        } catch {
          case _: Throwable => // Manejar cualquier excepción
        }
        val fin = System.nanoTime()
        (fin - inicio).toDouble / 1e6  // Convertir a milisegundos
      }
      tiempos.sum / numEjecuciones  // Promedio de tiempos
    }

    val tiempoFuncion1 = medirTiempo(funcion1)
    val tiempoFuncion2 = medirTiempo(funcion2)

    val aceleracion = tiempoFuncion1 / tiempoFuncion2

    (tiempoFuncion1, tiempoFuncion2, aceleracion)
  }

  def generarCadenaAleatoria(tamanio: Int, alfabeto: Seq[Char]): String = {
    val random = new Random
    (1 to tamanio).map(_ => alfabeto(random.nextInt(alfabeto.length))).mkString
  }

  def main(args: Array[String]): Unit = {
    val alfabeto = "acgt".toSeq
    
  
    
    // Comparaciones de rendimiento para cada función
    for (tamanioCadena <- 2 to 14) {
      val cadenaObjetivo = generarCadenaAleatoria(tamanioCadena, alfabeto)
      val buscadorCadena1 = new BuscadorCadena(cadenaObjetivo, alfabeto)
      //val buscadorCadena2 = new BuscadorCadena(cadenaObjetivo, alfabeto)

      println(s"Pruebas con cadena de tamaño $tamanioCadena")

      println("Pruebas PRC_Ingenuo vs. PRC_IngenuoPar")
      val resultadosIngenuo = compararAlgoritmos(buscadorCadena1.PRC_Ingenuo(), buscadorCadena1.PRC_IngenuoPar())
      println(resultadosIngenuo)

      println("Pruebas PRC_Mejorado vs. PRC_turbo_mejorada")
      val resultadosMejorado = compararAlgoritmos(buscadorCadena1.PRC_Mejorado(), buscadorCadena1.PRC_MejoradoPar())
      println(resultadosMejorado)

      println("Pruebas PRC_turbo vs. PRC_turboPar")
      val resultadosTurbo = compararAlgoritmos(buscadorCadena1.PRC_turbo(), buscadorCadena1.PRC_turboPar())
      println(resultadosTurbo)


      println("Pruebas PRC_turbo_mejorada vs. PRC_turbo_mejoradaPar")
      val resultadosTurboMejorada = compararAlgoritmos(buscadorCadena1.PRC_turbo_mejorada(), buscadorCadena1.PRC_turbo_mejoradaPar())
      println(resultadosTurboMejorada)

      println("Pruebas turboAcelerada vs. turboAceleradaPar")
      val resultadosTurboAcelerada = compararAlgoritmos(buscadorCadena1.turboAcelerada(), buscadorCadena1.turboAceleradaPar())
      println(resultadosTurboAcelerada)

      println()
    }
    
  }
}