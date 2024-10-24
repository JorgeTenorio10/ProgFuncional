

object Main extends ArbolHuffman {
  def main(args: Array[String]): Unit = {
    val hojaS = HojaHuff('S', 4)
    val hojaO = HojaHuff('O', 3)
    val hojaE = HojaHuff('E', 2)
    val hojaEspacio = HojaHuff(' ', 2)

    val rama1 = RamaHuff(hojaE, hojaEspacio)
    val rama2 = RamaHuff(rama1, hojaO)
    val arbolHuffman = RamaHuff(hojaS, rama2)

    print(s"Peso total del árbol: ${peso  (arbolHuffman)}")

  }

}
