
object Main extends ArbolHuffman {
  def main(args: Array[String]): Unit = {
    val hojaS = HojaHuff('S', 4)
    val hojaO = HojaHuff('O', 3)
    val hojaE = HojaHuff('E', 2)
    val hojaEspacio = HojaHuff(' ', 2)
    val rama1 = RamaHuff(hojaE, hojaEspacio)
    val rama2 = RamaHuff(rama1, hojaO)
    val arbolHuffman = RamaHuff(hojaS, rama2)
    val listabits:List[Bit]=List(0,1,0,0,1,1,1,1,1,0,0,1,1,0,1,1,1,1,0,0,1,0)
    val listaletras:String= "SO ES"


    print(s"Peso total del árbol: ${peso  (arbolHuffman)}\n")
    print(s"Caracteres del árbol: ${caracteres(arbolHuffman)}\n")
    print(s"cadena 0100101101:${arbolHuffman.decodificar(listabits)}\n")
    print(s"El arbol contiene el caracter:${arbolHuffman.contieneCaracter(" ".charAt(0))}\n")
    print(s"cadena SO ES:${arbolHuffman.codificar(listaletras)}\n")
    val lista=ListaCharsADistFrec(cadenaAListaChars("Pepe tiene tres pelos"))
    val lista2:String="Pepe tiene tres pelos"
    print(s"Lista de frecuencias de la lista de arriba ${lista}\n")
    print(s"Lista de frecuencias ordenada y pasado a hojas ${DistribFrecAListaHojas(lista)}\n")
    val miArbol= ArbolHuffman("texto para construir el arbol huffman")
    print(s"arboleado ${miArbol}")
  }

}
