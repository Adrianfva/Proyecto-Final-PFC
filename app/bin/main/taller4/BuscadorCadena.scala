/**
 * Clase BuscadorCadena
 */

package taller4
import taller4.TrieDefinitions._
import common._
//import scala.concurrent.forkjoin._

//import scala.collection.parallel._
import scala.annotation.tailrec


import scala.concurrent.{Future, ExecutionContext, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


//import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParVector

import scala.collection.parallel._
import scala.collection.parallel.CollectionConverters._


/**
 * Clase que define diferentes algoritmos para encontrar una cadena en otra.
 *
 * @param cadenaObjetivo Cadena objetivo a buscar.
 * @param alfabeto Alfabeto de caracteres disponibles.
 */
class BuscadorCadena(cadenaObjetivo: String, alfabeto: Seq[Char]) {
  
  // Definición de tipo para el oráculo que verifica la existencia de subcadenas.
  type Oraculo = Seq[Char] => Boolean
  
  // Oráculo que evalúa si una subcadena está presente en la cadena objetivo.
  val oraculo: Oraculo = (subcadena: Seq[Char]) => cadenaObjetivo.contains(subcadena.mkString)
  
  // Longitud de la cadena objetivo.
  val n: Int = cadenaObjetivo.length

  /**
   * Genera todas las combinaciones posibles al concatenar una lista de cadenas con otra lista de cadenas.
   *
   * @param lista Lista de combinaciones existentes.
   * @param listaCadenasAconcatenar Lista de cadenas a concatenar.
   * @param acc Lista acumulativa de combinaciones generadas.
   * @return Lista de todas las combinaciones posibles.
   */
  def agregarcombinaciones(lista: Seq[Seq[Char]], listaCadenasAconcatenar: Seq[Char], acc: Seq[Seq[Char]] = Seq(Seq())): Seq[Seq[Char]] = {
    @tailrec
    def agregarcombinacionesRec(lista: Seq[Seq[Char]], listaCadenas: Seq[Char], acc: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (lista.isEmpty) acc.tail
      else if (lista.head.isEmpty) agregarcombinaciones(lista.tail, listaCadenasAconcatenar, acc)
      else {
        val nuevasCombinaciones = listaCadenasAconcatenar.map(cadena => lista.head :+ cadena)
        agregarcombinacionesRec(lista.tail, listaCadenasAconcatenar, acc ++ nuevasCombinaciones)
      }
    }
    agregarcombinacionesRec(lista, listaCadenasAconcatenar, acc)
  }

  /**
   * Genera todas las combinaciones posibles al concatenar una cadena con ella misma múltiples veces.
   *
   * @param cadenaAconcatenar Cadena a concatenar.
   * @param tamañodeseado Tamaño deseado de las combinaciones.
   * @param lista Lista de combinaciones existentes.
   * @return Lista de todas las combinaciones posibles.
   */
  def gemerarCombinaciones(caracteres: Seq[Char], longitud: Int): Seq[Seq[Char]] = {
    def subcadenas(subs: Seq[Seq[Char]], n: Int): Seq[Seq[Char]] = {
      if (n > longitud) subs
      else subcadenas(subs.flatMap(subcadena => caracteres.map(subcadena :+ _)), n + 1)
    }

    subcadenas(caracteres.map(Seq(_)), 2)
  }


  /**
   * Algoritmo de búsqueda PRC (Pattern Recognition Code) Ingenuo.
   *
   * @return Opción que contiene la primera subcadena encontrada, si existe.
   */
  def PRC_Ingenuo(): Option[Seq[Char]] = {
    val alfabetoenForma = alfabeto.map(cadena => Seq(cadena))
    val combinaciones = gemerarCombinaciones(alfabeto, n)
    combinaciones.find(oraculo)
  }

  /**
   * Algoritmo de búsqueda PRC (Pattern Recognition Code) Mejorado.
   *
   * @return Lista de subcadenas encontradas.
   */
  def PRC_Mejorado(): Seq[Seq[Char]]  = {
    def PRC_MejoradoAux(sck: Seq[Seq[Char]]): Seq[Seq[Char]]  = {
      if (sck.isEmpty) Seq(Seq()) 
      else {
        val combinaciones = agregarcombinaciones(sck, alfabeto).filter(oraculo(_))
        if (combinaciones.exists(_.size == n)) combinaciones
        else PRC_MejoradoAux(combinaciones) 
      }
    }
    val alfabetoenForma = alfabeto.map(cadena => Seq(cadena))
    PRC_MejoradoAux(alfabetoenForma)
  }

    /**
   * Combina dos listas de cadenas generando todas las posibles combinaciones.
   *
   * @param lista1 Primera lista de cadenas.
   * @param lista2 Segunda lista de cadenas.
   * @return Lista de todas las combinaciones posibles.
   */
  def combinar(lista1: Seq[Seq[Char]], lista2: Seq[Seq[Char]]): Seq[Seq[Char]] = { 
    if (lista1.isEmpty) Nil 
    else lista2.map { cadena => cadena ++ lista1.head } ++ combinar(lista1.tail, lista2)
  }

  /**
   * Algoritmo de búsqueda PRC (Pattern Recognition Code) Turbo.
   *
   * @param nA Tamaño deseado de las subcadenas a buscar.
   * @return Lista de subcadenas encontradas.
   */
  def PRC_turbo(nA: Int = n): Seq[Seq[Char]] = {
    def PRC_turboAux(sck: Seq[Seq[Char]], n: Int,cadedaPasada: Seq[Seq[Char]] = Seq(Seq()) ): Seq[Seq[Char]]  = {
      if (sck.head.size + 1 == n) {
        val combinaciones = agregarcombinaciones(sck, sck.flatten.distinct)
        combinaciones.filter(oraculo(_))
      } else if (sck.head.size >= n) sck
      else if (sck.head.size*2 > n) {
        val combinaciones = combinar(sck, cadedaPasada).filter(oraculo(_))
        PRC_turboAux(combinaciones, n,cadedaPasada) 
      }
      else {
        val combinaciones = combinar(sck, sck).filter(oraculo(_))
        if (combinaciones.filter(_.size == n).size != 0) combinaciones
        else PRC_turboAux(combinaciones, n,sck) 
      }
    }
    val alfabetoenForma = alfabeto.map(cadena => Seq(cadena))
    if (nA == 1) alfabetoenForma.filter(oraculo(_))
    else if (nA % 2 == 0) {
      val sub_cadenas = PRC_turboAux(alfabetoenForma, nA / 2)
      val combinaciones = combinar(sub_cadenas, sub_cadenas).filter(oraculo(_))
      combinaciones
    } 
    else {
      val alfabeto_meno = alfabeto.map(cadena => Seq(cadena))
      val cadenas1 = PRC_turboAux(alfabetoenForma, (nA + 1) / 2)
      val cadenas2 = PRC_turboAux(alfabetoenForma, nA / 2)
      val combinaciones = combinar(cadenas1, cadenas2).filter(oraculo(_))
      combinaciones
    }
  }

  /**
   * Filtra las cadenas que cumplen con ciertas condiciones.
   *
   * @param s Cadena a filtrar.
   * @param sck Lista de subcadenas.
   * @param k Longitud de subcadena a verificar.
   * @return Booleano que indica si la cadena cumple con las condiciones.
   */
  def filtro_cadenas(s: Seq[Char], sck: Seq[Seq[Char]], k: Int): Boolean = {
    def estaContenido(s: Seq[Char], sck: Seq[Seq[Char]]): Boolean = {
      if (sck.isEmpty) false
      else if (sck.head == s) true
      else estaContenido(s, sck.tail)
    }
    if (s.length == k) estaContenido(s.take(k), sck)
    else {
      val subcadena = s.take(k)
      if (estaContenido(subcadena, sck)) filtro_cadenas(s.tail, sck, k)
      else false
    } 
  }

  /**
   * Algoritmo de búsqueda PRC (Pattern Recognition Code) Turbo Mejorado.
   *
   * @param nArg Tamaño deseado de las subcadenas a buscar.
   * @return Lista de subcadenas encontradas.
   */
  def PRC_turbo_mejorada(nArg: Int = n): Seq[Seq[Char]]  = {
    def PRC_turbo_mejoradaAux(sck: Seq[Seq[Char]], n: Int,cadedaPasada: Seq[Seq[Char]] = Seq(Seq()) ): Seq[Seq[Char]]  = {
      if (sck.isEmpty) Seq() 
      else if (sck.head.size >= n) sck
      else if (sck.head.size + 1 == n ) {
        val combinaciones = agregarcombinaciones(sck, sck.flatten.distinct)
        combinaciones.filter(oraculo(_))
      } else if (sck.head.size + 1 == n ) {
        val combinaciones = combinar(sck, cadedaPasada).filter(filtro_cadenas(_, cadedaPasada, cadedaPasada.head.size)).filter(oraculo(_))
        PRC_turbo_mejoradaAux(combinaciones, n,cadedaPasada) 
      } else {
        val combinaciones = combinar(sck, sck).filter(filtro_cadenas(_, sck, sck.head.size)).filter(oraculo(_))
        if (combinaciones.filter(_.size == n).size != 0) combinaciones
        else PRC_turbo_mejoradaAux(combinaciones, n,sck) 
      }
    }
    val alfabetoenForma = alfabeto.map(cadena => Seq(cadena))
    if (nArg == 1) alfabetoenForma.filter(oraculo(_))
    else if (nArg % 2 == 0) {
      val sub_cadenas = PRC_turbo_mejoradaAux(alfabetoenForma, nArg / 2)
      val combinaciones = combinar(sub_cadenas, sub_cadenas).filter(oraculo(_))
      combinaciones
    } else {
      val cadenas1 = PRC_turbo_mejoradaAux(alfabetoenForma, (nArg + 1) / 2)
      val cadenas2 = PRC_turbo_mejoradaAux(alfabetoenForma, nArg / 2)
      val combinaciones = combinar(cadenas1, cadenas2).filter(oraculo(_))
      combinaciones
    }
  }

  /**
   * Verifica si una cadena pertenece a un árbol de sufijos.
   *
   * @param c Cadena a verificar.
   * @param t Árbol de sufijos.
   * @return Booleano que indica si la cadena pertenece al árbol.
   */
  def pertenece(c: Seq[Char], t: Trie): Boolean = {
    t match {
      case Nodo(car, marcada, hijos) => {
        if (c.isEmpty) marcada
        else {
          val cabeza = c.head
          val cola = c.tail
          val hijosCabeza = hijos.filter(hijo => raiz(hijo) == cabeza)
          if (hijosCabeza.isEmpty) false
          else pertenece(cola, hijosCabeza.head)
        }
      }
      case Hoja(car, marcada) => {
        if (c.isEmpty) marcada
        else false
      }
    }
  }

  /**
   * Agrega una cadena al árbol de sufijos.
   *
   * @param c Cadena a agregar.
   * @param t Árbol de sufijos.
   * @return Árbol de sufijos modificado.
   */
  def agregar(c: Seq[Char], t: Trie): Trie = {
    t match {
      case Nodo(car, marcada, hijos) => {
        if (c.isEmpty) Nodo(car, true, hijos)
        else {
          val cabeza = c.head
          val cola = c.tail
          val hijosCabeza = hijos.filter(hijo => raiz(hijo) == cabeza)
          if (hijosCabeza.isEmpty) {
            val nuevoHijo = if (cola.isEmpty) Hoja(cabeza, true) else agregar(cola, Nodo(cabeza, false, List()))
            Nodo(car, marcada, nuevoHijo :: hijos)
          } else {
            val nuevoHijo = agregar(cola, hijosCabeza.head)
            val otrosHijos = hijos.filter(hijo => raiz(hijo) != cabeza)
            Nodo(car, marcada, nuevoHijo :: otrosHijos)
          }
        }
      }
      case Hoja(car, marcada) => {
        if (c.isEmpty) Hoja(car, true)
        else {
          val cabeza = c.head
          val cola = c.tail
          if (car == cabeza) Nodo(car, marcada, List(if (cola.isEmpty) Hoja(cabeza, true) else agregar(cola, Nodo(cabeza, false, List()))))
          else Nodo(car, marcada, List(if (cola.isEmpty) Hoja(cabeza, true) else agregar(cola, Nodo(cabeza, false, List()))))
        }
      }
    }
  }

  /**
   * Agrega una secuencia de cadenas al árbol de sufijos.
   *
   * @param ss Lista de secuencias a agregar.
   * @param t Árbol de sufijos.
   * @return Árbol de sufijos modificado.
   */
  def agregarsS_arbol(ss: Seq[Seq[Char]], t: Trie): Trie = {
    if (ss.isEmpty) t
    else agregarsS_arbol(ss.tail, agregar(ss.head, t))
  }

  /**
   * Construye un árbol de sufijos a partir de una lista de secuencias de cadenas.
   *
   * @param ss Lista de secuencias de cadenas.
   * @return Árbol de sufijos construido.
   */
  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    val t: Trie = agregarsS_arbol(ss, Nodo(' ', false, List()))
    t
  }

  
  /**
   * Filtra las cadenas que cumplen con ciertas condiciones en el árbol de sufijos.
   *
   * @param s Cadena a filtrar.
   * @param t Árbol de sufijos.
   * @param k Longitud de subcadena a verificar.
   * @return Booleano que indica si la cadena cumple con las condiciones.
   */
  def filtro_arbol(s: Seq[Char], t: Trie, k: Int): Boolean = {
    if (s.length == k) pertenece(s, t)
    else {
      val subcadena = s.take(k)
      if (pertenece(subcadena, t)) filtro_arbol(s.tail, t, k)
      else false
    } 
  }


  /**
   * Recorre un árbol de sufijos y obtiene todas las secuencias presentes.
   *
   * @param t Árbol de sufijos.
   * @return Lista de secuencias presentes en el árbol.
   */
  def recorrerArbol(t: Trie): Seq[Seq[Char]] = {
    t match {
      case Nodo(c, _, hijos) => 
        val secuenciasHijos = hijos.flatMap(hijo => recorrerArbol(hijo))
        secuenciasHijos.map(secuencia => c +: secuencia).map(_.filter(_ != ' '))
      case Hoja(c, _) => Seq(Seq(c))
    }
  }

/**
  * Función auxiliar ramaValida que verifica si una rama tiene hijos.
  *
  * @param t: Trie: Nodo o Hoja del árbol de sufijos.
  * @return Boolean: true si la rama tiene hijos, false en caso contrario.
  */
  def ramaValida(t: Trie): Boolean = {
    t match {
      case Nodo(_, _, hijos) => hijos.nonEmpty
      case Hoja(_, _) => true
    }
  }

  /**
   * Algoritmo de búsqueda PRC (Pattern Recognition Code) Turbo Acelerada.
   *
   * @return Lista de subcadenas encontradas.
   */
  def turboAcelerada(): Seq[Seq[Char]] = {
    
  /**
    * Función auxiliar turboAceleradaParAux que realiza la reconstrucción de secuencias en paralelo.
    *
    * @param t: Trie: Árbol de sufijos.
    * @param seqACombinar: Seq[Seq[Char]]: Secuencias a combinar.
    * @param secAcc: Seq[Char]: Secuencia acumulada.
    * @return Trie: Árbol de sufijos actualizado.
    */
  
    def turboAceleradaAux(t: Trie, seqACombinar: Seq[Seq[Char]], secAcc: Seq[Char]): Trie = {
      t match {
        case Nodo(valor, esFinal, hijos) =>
          val nuevosHijos = hijos.map { hijo =>
            if (ramaValida(hijo)) turboAceleradaAux(hijo, seqACombinar, secAcc :+ valor)
            else hijo
          }
          Nodo(valor, esFinal, nuevosHijos)
        case Hoja(valor, esFinal) =>
          if (!esFinal) Nodo(valor, esFinal, List())
          else {
            val cadenasNuevas: Seq[Seq[Char]] = seqACombinar.map(cadena => (secAcc.filter(_ != ' ') :+ valor) ++ cadena).filter(filtro_cadenas(_, seqACombinar, seqACombinar.head.size)).filter(oraculo(_))
            if (cadenasNuevas.isEmpty) Nodo(valor, esFinal, List())
            else {
              agregarsS_arbol(cadenasNuevas.map(_.drop((secAcc.filter(_ != ' ') :+ valor).length)), t)
            }
          }
      }
    }

    /**
      * Función auxiliar evaluar_arbol que evalúa y actualiza el árbol de sufijos.
      *
      * @param t: Trie: Árbol de sufijos.
      * @param n: Int: Longitud de las secuencias.
      * @return Seq[Seq[Char]]: Secuencias encontradas que cumplen con el oráculo.
      */
    def evaluar_arbol(t: Trie, n: Int): Seq[Seq[Char]] = {
      val todas_secuencias_arbol = recorrerArbol(t)
      if (todas_secuencias_arbol.head.length + 1 == n) agregarcombinaciones(todas_secuencias_arbol, alfabeto).filter(oraculo(_))
      else if (todas_secuencias_arbol.head.size == n) todas_secuencias_arbol
      else {
        val tnew = turboAceleradaAux(t, todas_secuencias_arbol, Seq())
        evaluar_arbol(tnew, n)
      }
    }

    val arbol_inicial = arbolDeSufijos(alfabeto.map(cadena => Seq(cadena)))
    if (n % 2 == 0) {
      val sub_cadenas = evaluar_arbol(arbol_inicial, n / 2)
      val combinaciones = combinar(sub_cadenas, sub_cadenas).filter(oraculo(_))
      combinaciones
    } else {
      val c1 = evaluar_arbol(arbol_inicial, (n + 1) / 2)
      val c2 = evaluar_arbol(arbol_inicial, (n - 1) / 2)
      val combinaciones = combinar(c1, c2).filter(oraculo(_))
      combinaciones
    }
  }
  //
  //    funciones paralelas
  //
  //
  
/**
  * Función paralela PRC_IngenuoPar que busca secuencias usando combinaciones y paralelismo.
  *
  * @return Option[Seq[Char]]: Secuencia encontrada que cumple con el oráculo, si existe.
  */
  def PRC_IngenuoPar(): Option[Seq[Char]] = {
    val alfabetoenForma = alfabeto.map(cadena => Seq(cadena))
    val resultado = gemerarCombinaciones(alfabeto, n )
    val (c1, c2) = parallel(resultado.take(n / 2).find(oraculo), resultado.drop(n / 2).find(oraculo))
    if (c1.isEmpty) c2 else c1

  }

  /**
    * Función paralela PRC_MejoradoPar que busca secuencias mejoradas utilizando combinaciones y paralelismo.
    *
    * @return Seq[Seq[Char]]: Conjunto de secuencias encontradas que cumplen con el oráculo.
  */
  def PRC_MejoradoPar(): Seq[Seq[Char]] = {
    def PCR_Mejorajo_sub_cadena(subsck: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      val c1 = agregarcombinaciones(subsck.take(n / 2), alfabeto).find(oraculo)
      if (c1.isDefined) c1.toSeq else Seq(Seq())
    }

    def PRC_MejoradoParAux(sck: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (sck.head.isEmpty) Seq(Seq())
      else {
        val (c1, c2) = parallel(PCR_Mejorajo_sub_cadena(sck.take(sck.length / 2)), PCR_Mejorajo_sub_cadena(sck.drop(sck.length / 2)))
        val total = c1 ++ c2
        if (total.filter(_.size == n).nonEmpty) total
        else PRC_MejoradoParAux(total)
      }
    }

    val alfabetoenForma = alfabeto.map(cadena => Seq(cadena))
    PRC_MejoradoParAux(alfabetoenForma)
  }

/**
  * Función paralela PRC_turboPar que busca secuencias utilizando combinaciones y paralelismo de manera iterativa.
  *
  * @param na: Int = n: Longitud de las secuencias a buscar (por defecto, igual a n).
  * @return Seq[Seq[Char]]: Conjunto de secuencias encontradas que cumplen con el oráculo.
  */
  def PRC_turboPar(na: Int = n): Seq[Seq[Char]] = {
    if (na == 2) PRC_turbo(na)
    else if (na % 2 == 0) {
      val sub_cadenas = PRC_turboPar(na - 1)
      val combinaciones = agregarcombinaciones(sub_cadenas, alfabeto).filter(oraculo(_))
      combinaciones
    } else {
      val cadenas1 = task { PRC_turbo((na + 1) / 2) }
      val cadenas2 = task { PRC_turbo((na - 1) / 2) }
      val combinaciones = combinar(cadenas1.join, cadenas2.join).filter(oraculo(_))
      combinaciones
    }
  }


  /**
  * Función PRC_turbo_mejoradaPar que utiliza paralelismo para acelerar la reconstrucción de secuencias.
  *
  * @param na: Int: Longitud de las secuencias.
  * @return Seq[Seq[Char]]: Secuencias reconstruidas que cumplen con el oráculo.
  */
  def PRC_turbo_mejoradaPar(na: Int = n): Seq[Seq[Char]]  = {
    if (na == 2) PRC_turbo(na)
    else if (na % 2 == 0) {
      val sub_cadenas = PRC_turbo_mejoradaPar(na - 1)
      val combinaciones = agregarcombinaciones(sub_cadenas, alfabeto).filter(oraculo(_))
      combinaciones
    } else {
      val cadenas1 = task { PRC_turbo_mejorada((na + 1) / 2) }
      val cadenas2 = task { PRC_turbo_mejorada((na - 1) / 2) }
      val combinaciones = combinar(cadenas1.join, cadenas2.join).filter(oraculo(_))
      combinaciones
    }
  }

  
/**
  * Función turboAceleradaPar que reconstruye secuencias utilizando la propiedad de concatenación de subsecuencias y paralelismo.
  *
  * @return Seq[Seq[Char]]: Secuencias reconstruidas que cumplen con el oráculo.
  */
  def turboAceleradaPar(): Seq[Seq[Char]] = {
  /**
    * Función auxiliar turboAceleradaParAux que realiza la reconstrucción de secuencias en paralelo.
    *
    * @param t: Trie: Árbol de sufijos.
    * @param seqACombinar: Seq[Seq[Char]]: Secuencias a combinar.
    * @param secAcc: Seq[Char]: Secuencia acumulada.
    * @return Trie: Árbol de sufijos actualizado.
    */
  
    def turboAceleradaParAux(t: Trie, seqACombinar: Seq[Seq[Char]], secAcc: Seq[Char]): Trie = {
      t match {
        case Nodo(valor, esFinal, hijos) =>
          val parVector: ParVector[Trie] = hijos.toVector.par
          val nuevosHijos = parVector.map { hijo =>
            if (ramaValida(hijo)) turboAceleradaParAux(hijo, seqACombinar, secAcc :+ valor)
            else hijo
          }
          Nodo(valor, esFinal, nuevosHijos.seq.toList)
        case Hoja(valor, esFinal) =>
          if (!esFinal) Nodo(valor, esFinal, List())
          else {
            val cadenasNuevas: Seq[Seq[Char]] = seqACombinar.map(cadena => (secAcc.filter(_ != ' ') :+ valor) ++ cadena).filter(filtro_cadenas(_, seqACombinar, seqACombinar.head.size)).filter(oraculo(_))
            if (cadenasNuevas.isEmpty) Nodo(valor, esFinal, List())
            else {
              agregarsS_arbol(cadenasNuevas.map(_.drop((secAcc.filter(_ != ' ') :+ valor).length)), t)
            }
          }
      }
    }

    /**
      * Función auxiliar evaluar_arbol que evalúa y actualiza el árbol de sufijos.
      *
      * @param t: Trie: Árbol de sufijos.
      * @param n: Int: Longitud de las secuencias.
      * @return Seq[Seq[Char]]: Secuencias encontradas que cumplen con el oráculo.
      */
    def evaluar_arbol(t: Trie, n: Int): Seq[Seq[Char]] = {
      val todas_secuencias_arbol = recorrerArbol(t)
      if (todas_secuencias_arbol.head.length + 1 == n) agregarcombinaciones(todas_secuencias_arbol, alfabeto).filter(oraculo(_))
      else if (todas_secuencias_arbol.head.size == n) todas_secuencias_arbol
      else {
        val tnew = turboAceleradaParAux(t, todas_secuencias_arbol, Seq())
        evaluar_arbol(tnew, n)
      }
    }

    val arbol_inicial = arbolDeSufijos(alfabeto.map(cadena => Seq(cadena)))
    if (n % 2 == 0) {
      val sub_cadenas = evaluar_arbol(arbol_inicial, n / 2)
      val combinaciones = combinar(sub_cadenas, sub_cadenas).filter(oraculo(_))
      combinaciones
    } else {
      val c1 = evaluar_arbol(arbol_inicial, (n + 1) / 2)
      val c2 = evaluar_arbol(arbol_inicial, (n - 1) / 2)
      val combinaciones = combinar(c1, c2).filter(oraculo(_))
      combinaciones
    }
  }
}


  

