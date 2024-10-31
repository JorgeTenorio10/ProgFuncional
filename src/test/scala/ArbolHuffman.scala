import scala.annotation.tailrec

type Bit = 0 | 1

def cadenaAListaChars(cadena:String):List[Char]=
  cadena.toList

def listaCharsACadena(caracteres:List[Char]):String=
  caracteres.mkString

def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] = {
  def contarCaracteres(lista: List[Char], acumulado: List[(Char, Int)]): List[(Char, Int)] = {
    lista match {
      case Nil => acumulado // Si la lista está vacía, devolvemos el acumulado actual
      case cabeza :: cola =>
        val (existe, actualizado) = actualizarFrecuencia(cabeza, acumulado)
        if (existe) contarCaracteres(cola, actualizado)
        else contarCaracteres(cola, (cabeza, 1) :: acumulado)
    }
  }

  def actualizarFrecuencia(caracter: Char, acumulado: List[(Char, Int)]): (Boolean, List[(Char, Int)]) = {
    acumulado match {
      case Nil => (false, Nil)
      case (car, freq) :: resto =>
        if (car == caracter) (true, (car, freq + 1) :: resto) // Actualizamos la frecuencia
        else {
          val (encontrado, listaActualizada) = actualizarFrecuencia(caracter, resto)
          (encontrado, (car, freq) :: listaActualizada)
        }
    }
  }

  contarCaracteres(listaChar, Nil)
}

abstract class ArbolHuffman {
  def peso(arbol: ArbolHuffman): Int = arbol match
    case HojaHuff(_, peso) => peso
    case RamaHuff(nodoIzq, nodoDcho) => peso(nodoIzq) + peso(nodoDcho)
  def contieneCaracter(caracter:Char): Boolean=
    def contieneCaracterAux(caracter:Char,caracteres:List[Char]): Boolean= caracteres match
      case caracterArbol::res if caracterArbol==caracter =>true
      case caracterArbol::res =>contieneCaracterAux(caracter,res)
      case Nil => false
    contieneCaracterAux(caracter,this.caracteres(this))
  
  def caracteres(arbol: ArbolHuffman): List[Char] = arbol match
    case HojaHuff(caracter, _) => List(caracter)
    case RamaHuff(nodoIzq, nodoDcho) => caracteres(nodoIzq) ++ caracteres(nodoDcho)

  def decodificar(bits: List[Bit]): String =
    @annotation.tailrec
    def decodificarAux(actual: ArbolHuffman, bitsRestantes: List[Bit], codigo: List[Char]): List[Char] = (actual, bitsRestantes) match
      case (_, Nil) => codigo

      case (hojaHuff: HojaHuff, _) => decodificarAux(this, bitsRestantes, codigo :+ hojaHuff.caracter)

      case (ramaHuff: RamaHuff, 0 :: bitsRestantes) => decodificarAux(ramaHuff.nodoIzq, bitsRestantes, codigo)

      case (ramaHuff: RamaHuff, 1 :: bitsRestantes) => decodificarAux(ramaHuff.nodoDch, bitsRestantes, codigo)

    listaCharsACadena(decodificarAux(this, bits, List.empty[Char]))

  def codificarCaracter(caracter: Char, arbol: ArbolHuffman, camino: List[Bit]): List[Bit] = arbol match
    case HojaHuff(c, _) if c == caracter => camino
    case RamaHuff(nodoIzq, nodoDch) =>
      if (nodoIzq.contieneCaracter(caracter)) codificarCaracter(caracter, nodoIzq, camino :+ 0)
      else codificarCaracter(caracter, nodoDch, camino :+ 1)
    case _ if contieneCaracter(caracter)==false => throw new IllegalArgumentException("Carácter no encontrado en el árbol")


  def codificar(cadena: String): List[Bit] =
    @tailrec
    def codificarAux(resto: List[Char], resultado: List[Bit]): List[Bit] = resto match
      case Nil => resultado
      case caracter :: restoCadena => val codigoCaracter = codificarCaracter(caracter, this, List.empty[Bit])
        codificarAux(restoCadena, resultado ++ codigoCaracter)

    codificarAux(cadenaAListaChars(cadena), List.empty[Bit])
}


  case class RamaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman
      
  case class HojaHuff(caracter: Char, peso: Int) extends ArbolHuffman

