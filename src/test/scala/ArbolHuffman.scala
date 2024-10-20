abstract class ArbolHuffman

case class RamaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman
      
case class HojaHuff(caracter: Char, peso: Int) extends ArbolHuffman

