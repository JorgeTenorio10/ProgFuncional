import scala.annotation.tailrec

type Bit = 0 | 1

def cadenaAListaChars(cadena:String):List[Char]=
  cadena.toList

def listaCharsACadena(caracteres:List[Char]):String=
  caracteres.mkString


abstract class ArbolHuffman {
  def peso(arbol: ArbolHuffman): Int = arbol match
    case HojaHuff(_, peso) => peso
    case RamaHuff(nodoIzq, nodoDcho) => peso(nodoIzq) + peso(nodoDcho)

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
}


  case class RamaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman
      
  case class HojaHuff(caracter: Char, peso: Int) extends ArbolHuffman

