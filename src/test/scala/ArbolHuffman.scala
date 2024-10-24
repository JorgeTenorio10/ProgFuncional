type Bit = 0 | 1

def cadenaAListaChars(cadena:String):List[Char]=
  cadena.toList

def listaCharsACadena(caracteres:List[Char]):String=
  caracteres.mkString


abstract class ArbolHuffman{
  def peso(arbol: ArbolHuffman): Int = arbol match
    case HojaHuff(_, peso) => peso
    case RamaHuff(nodoIzq, nodoDcho) => peso(nodoIzq) + peso(nodoDcho)

  def caracteres(arbol: ArbolHuffman): List[Char] = arbol match
    case HojaHuff(caracter, _) => List(caracter)
    case RamaHuff(nodoIzq, nodoDcho) => caracteres(nodoIzq) ++ caracteres(nodoDcho)
  def codificar(bits:List[Bit]):String=

}


case class RamaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman
      
case class HojaHuff(caracter: Char, peso: Int) extends ArbolHuffman

