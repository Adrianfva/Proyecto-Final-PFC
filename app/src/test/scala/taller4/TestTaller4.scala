/**
 * Plantilla para pruebas
* @author Adrian Felipe Velasquez Arias 202259456, Edgar Fabian Rueda Colonia 202259606
* @version 1.0
 */
package taller4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner
// Importar el objeto Taller4 que contiene la función multMatriz y las funciones auxiliares
import Taller4._

@RunWith(classOf[JUnitRunner])
class testMultMatriz extends AnyFunSuite {
  // Definir una función para mostrar las matrices de forma más legible

  test("Prueba1") {
    // Crear instancias de BuscadorCadena con dassertrentes parámetros de en
    val alfabeto = "acgt".toSeq
    for (tamanioCadena <- 2 to 5) {
        val cadenaObjetivo = generarCadenaAleatoria(tamanioCadena, alfabeto)
        val buscadorCadena1 = new BuscadorCadena(cadenaObjetivo, alfabeto)

        println(s"Pruebas con cadena de tamaño $tamanioCadena")

        // Caso de prueba 1: PRC_Ingenuo
        val resultadoIngenuo = buscadorCadena1.PRC_Ingenuo()
        println(resultadoIngenuo)
        println(tamanioCadena)
        assert(resultadoIngenuo.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

        // Caso de prueba 2: PRC_Mejorado
        val resultadoMejorado = buscadorCadena1.PRC_Mejorado()
        assert(resultadoMejorado.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

        // Caso de prueba 3: PRC_turbo
        val resultadoTurbo = buscadorCadena1.PRC_turbo()
        assert(resultadoTurbo.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

        // Caso de prueba 4: PRC_turbo_mejorada
        val resultadoTurboMejorada = buscadorCadena1.PRC_turbo_mejorada()
        assert(resultadoTurboMejorada.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

        // Caso de prueba 5: PRC_IngenuoPar
        val resultadoIngenuoPar = buscadorCadena1.PRC_IngenuoPar()
        assert(resultadoIngenuoPar.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

        // Caso de prueba 6: PRC_turbo_mejoradaPar
        val resultadoTurboMejoradaPar = buscadorCadena1.PRC_turbo_mejoradaPar()
        assert(resultadoTurboMejoradaPar.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq) 

        // Caso de prueba 7: turboAceleradaPar
        val resultadoTurboAceleradaPar = buscadorCadena1.turboAceleradaPar()
        assert(resultadoTurboAceleradaPar.headOption.getOrElse(Seq()) == cadenaObjetivo.toSeq) 
        println()
      }
  }
}