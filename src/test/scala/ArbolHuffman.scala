import scala.annotation.tailrec

type Bit = 0 | 1
type TablaCodigos = List[(Char,List[Bit])]

abstract class ArbolHuffman {
  def peso(arbol: ArbolHuffman): Int = arbol match
    //Si estamos en una hoja cogemos su peso, si estamos en una rama sumamos los pesos de sus nodos
    case HojaHuff(_, peso) => peso
    case RamaHuff(nodoIzq, nodoDcho) => peso(nodoIzq) + peso(nodoDcho)
  def contieneCaracter(caracter:Char): Boolean=
    @tailrec
    def contieneCaracterAux(caracter:Char, caracteres:List[Char]): Boolean= caracteres match

      case caracterArbol::res if caracterArbol==caracter =>true
      case caracterArbol::res =>contieneCaracterAux(caracter,res)
      case Nil => false
    contieneCaracterAux(caracter,this.caracteres(this))
  
  def caracteres(arbol: ArbolHuffman): List[Char] = arbol match
    case HojaHuff(caracter, _) => List(caracter)
    case RamaHuff(nodoIzq, nodoDcho) => caracteres(nodoIzq) ++ caracteres(nodoDcho)

  def decodificar(bits: List[Bit]): String =
    @annotation.tailrec
    def decodificarAux(actual: ArbolHuffman, bitsRestantes: List[Bit], codigo: List[Char]): List[Char] =
      (actual, bitsRestantes) match
      //Si no quedan bits que decodificar devolvemos el codigo
      case (_, Nil) => codigo
      //Si quedan bits y estamos en una hoja añadimos el caracter de la hoja al codigo
      case (hojaHuff: HojaHuff, _) => decodificarAux(this, bitsRestantes, codigo :+ hojaHuff.caracter)
      //Si estamos en una rama y el bit siguiente es un 0 vamos al nodo izquierdo
      case (ramaHuff: RamaHuff, 0 :: bitsRestantes) => decodificarAux(ramaHuff.nodoIzq, bitsRestantes, codigo)
      //En el caso de que el bit siguiente sea un 1 vamos al nodo derecho
      case (ramaHuff: RamaHuff, 1 :: bitsRestantes) => decodificarAux(ramaHuff.nodoDch, bitsRestantes, codigo)
    //Lo convertimos en String
    listaCharsACadena(decodificarAux(this, bits, List.empty[Char]))

  def codificarCaracter(caracter: Char, arbol: ArbolHuffman, camino: List[Bit]): List[Bit] = arbol match
    //Si estamos en una hoja y contiene el caracter buscado, devuelve el camino acumulado
    case HojaHuff(c, _) if c == caracter => camino
    //Si es una rama vemos si el caracter esta en el subarbol izquierdo o derecho
    case RamaHuff(nodoIzq, nodoDch) =>
      if (nodoIzq.contieneCaracter(caracter)) codificarCaracter(caracter, nodoIzq, camino :+ 0)
      else codificarCaracter(caracter, nodoDch, camino :+ 1)
    //Excepcion si no esta el caracter en el arbol
    case _ if !contieneCaracter(caracter) => throw new IllegalArgumentException("Carácter no encontrado en el árbol")


  def codificar(cadena: String): List[Bit] =
    @tailrec
    def codificarAux(resto: List[Char], resultado: List[Bit]): List[Bit] = resto match
      //Si no quedan caracteres devuelve la lista de bits
      case Nil => resultado
      //Tomamos el primer caracter, lo codificamos y lo añadimos a la lista de bits
      case caracter :: restoCadena => val codigoCaracter = codificarCaracter(caracter, this, List.empty[Bit])
        codificarAux(restoCadena, resultado ++ codigoCaracter)

    codificarAux(cadenaAListaChars(cadena), List.empty[Bit])
}


  case class RamaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman
      
  case class HojaHuff(caracter: Char, peso: Int) extends ArbolHuffman

def cadenaAListaChars(cadena:String):List[Char]=
  cadena.toList
//convertimos un String en una lista de caracteres

def listaCharsACadena(caracteres:List[Char]):String=
  caracteres.mkString
//Convertimos una lista de caracteres en un String
def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] = {
  @tailrec
  def contarCaracteres(lista: List[Char], acumulado: List[(Char, Int)]): List[(Char, Int)] = {
    lista match {
      case Nil => acumulado // Si la lista está vacía, devolvemos el acumulado actual
      case cabeza :: cola =>
        val (existe, actualizado) = actualizarFrecuencia(cabeza, acumulado)
        if (existe) contarCaracteres(cola, actualizado)//si el elemento ya estaba en la lista, actualizamos la frecuencia
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
          (encontrado, (car, freq) :: listaActualizada)//añadimos un elemento el cual antes no tenia frecuencia
        }
    }
  }

  contarCaracteres(listaChar, Nil)
}

def DistribFrecAListaHojas(frec:List[(Char,Int)]):List[HojaHuff]=
  @tailrec
  def DistribFrecAListaHojasAux(frec:List[(Char,Int)], res:List[HojaHuff]):List[HojaHuff]= frec match
    //Si no quedan elementos ordena las hojas acumuladas por peso
    case Nil=> val hojasOrdenadas = res.sortBy(_.peso)
      hojasOrdenadas
    //Si hay mas elementos creamos una hoja con el primer elemento y le añadimos el resultado acumulado
    case (cabeza,peso)::resto=> val hojas=HojaHuff(cabeza,peso)::res
      DistribFrecAListaHojasAux(resto,hojas)

  DistribFrecAListaHojasAux(frec,Nil)

def crearRamaHuff(izq: ArbolHuffman, dch: ArbolHuffman): RamaHuff =
  RamaHuff(izq, dch) //construimos una rama
def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = nodos match
  case primero :: segundo :: resto =>
    //Hace una rama con el primer y segundo elemento de la lista y la añadimos a la lista
    val nuevaRama = crearRamaHuff(primero, segundo)
    val listaActualizada = (nuevaRama :: resto).sortBy(nodo => nodo.peso(nodo))
    listaActualizada
  case _ => nodos

def esListaSingleton(lista: List[ArbolHuffman]): Boolean =
  lista.length == 1
//vemos si es una lista Singleton

@tailrec
def repetirHasta(combinar: List[ArbolHuffman] => List[ArbolHuffman], esListaSingleton: List[ArbolHuffman] => Boolean)(lista: List[ArbolHuffman]): List[ArbolHuffman] =
  if (esListaSingleton(lista)) lista  //si es listaSingleton la devolvemos, sino lo repetimos hasta que lo sea
  else repetirHasta(combinar, esListaSingleton)(combinar(lista))

def crearArbolHuffman(cadena: String): ArbolHuffman =
  //Convertimos la cadena en lista de caracteres
  val listaChars = cadenaAListaChars(cadena)

  // Distribución de frecuencias de la lista de caracteres
  val distribucionFrecuencias = ListaCharsADistFrec(listaChars)

  // Convertimos la distribución de frecuencias en lista de hojas
  val listaHojas = DistribFrecAListaHojas(distribucionFrecuencias)

  // Creamos el árbolHuffman usando repetirHasta
  repetirHasta(combinar, esListaSingleton)(listaHojas).head


def deArbolATabla(arbol:ArbolHuffman):TablaCodigos=
  def deArbolATablaAux(arbol:ArbolHuffman,camino: List[Bit]): TablaCodigos= arbol match //funcion auxiliar de Arbol a tabla
    case HojaHuff(caracter,_)=>List((caracter,camino)) //si estamos en una hoja guardamos el caracter y su camino
    case RamaHuff(izq,dcho)=> deArbolATablaAux(izq,camino:+0)++ deArbolATablaAux(dcho,camino:+1)
  deArbolATablaAux(arbol,List())

def codificarCurri(tabla: TablaCodigos)(cadena: String): List[Bit] =
  @tailrec
  def buscarCodigo(caracter: Char, tabla: TablaCodigos): List[Bit] = tabla match
    // Si el carácter actual coincide con el buscado, devuelve su código en bits
    case (c, bits) :: _ if c == caracter => bits
    //En el caso contrario sigue buscando en el resto de la tabla
    case _ :: resto => buscarCodigo(caracter, resto)
    //Lanzamos excepcion si no se encuentra un caracter
    case Nil => throw new IllegalArgumentException(s"'$caracter' no se encuentra en la tabla")


  // Para cada carácter obtenemos el codigo y lo añadimos a la lista de bits
  @tailrec
  def codificarCurriAux(c: List[Char], res: List[Bit]): List[Bit] = c match
    case Nil => res
    //tomamos el código de bits del primer caracter y lo añadimos
    case c :: resto => val codigo = buscarCodigo(c, tabla)
      codificarCurriAux(resto, res ++ codigo)

  codificarCurriAux(cadena.toList, List())


def decodificarCurri(tabla: TablaCodigos)(bits: List[Bit]): String =
  // Función auxiliar para buscar el carácter que corresponde a la secuencia de bits inicial
  @tailrec
  def buscarCaracter(bits: List[Bit], tabla: TablaCodigos): (Char, List[Bit]) = tabla match
    //si los bits comienzan con el código buscado,devuelve el caracter y el resto de bits
    case (c, codigo) :: _ if empiezaCon(bits, codigo) => (c, bits.drop(codigo.length))
    //si no coincide sigue buscando
    case _ :: resto => buscarCaracter(bits, resto)
    //Excepcion por si no hay coincidencias
    case Nil => throw new IllegalArgumentException("Este código no está en la tabla")

  // Función auxiliar para comparar si una lista de bits comienza con otra
  @tailrec
  def empiezaCon(bits: List[Bit], prefijo: List[Bit]): Boolean =
    (bits, prefijo) match
      case (_, Nil) => true
      //Si coincide el elemento, continua comparando hasta ver si coinciden todos los elementos
      case (h :: t, he :: ta) if h == he => empiezaCon(t, ta)
      case _ => false

  // Decodifica recursivamente bits, construyendo la lista de caracteres decodificados
  @tailrec
  def decodificarCurriAux(bitsRestantes: List[Bit], resultado: List[Char]): List[Char] =
    if (bitsRestantes.isEmpty) resultado
    else
      //Busca el caracter que corresponde a los bits iniciales y el resto de bits por procesar
      val (caracter, nuevosBitsRestantes) = buscarCaracter(bitsRestantes, tabla)
      //llama de nuevo a la funcion con los bits restantes
      decodificarCurriAux(nuevosBitsRestantes, resultado :+ caracter)

  // Convertimos la lista de caracteres en una cadena final y la retornamos
  decodificarCurriAux(bits, List()).mkString

object Main extends ArbolHuffman {
  def main(args: Array[String]): Unit = {
    val hojaS = HojaHuff('S', 4)
    val hojaO = HojaHuff('O', 3)
    val hojaE = HojaHuff('E', 2)
    val hojaEspacio = HojaHuff(' ', 2)
    val rama1 = RamaHuff(hojaE, hojaEspacio)
    val rama2 = RamaHuff(rama1, hojaO)
    val arbolHuffman = RamaHuff(hojaS, rama2)
    val listabits:List[Bit]=List(0,1,0,0,0,1,1,1,0,1,0,1,1)
    val listaletras:String= "SO ES"
    val listabits2:List[Bit]=List(0,1,0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0)


    print(s"Peso total del árbol: ${peso  (arbolHuffman)}\n")
    print(s"Caracteres del árbol: ${caracteres(arbolHuffman)}\n")
    print(s"cadena 0100011101011:${arbolHuffman.decodificar(listabits)}\n")
    print(s"cadena SO ES:${arbolHuffman.codificar(listaletras)}\n")
    val lista=ListaCharsADistFrec(cadenaAListaChars("Pepe tiene tres pelos"))
    val lista2:String="Pepe tiene tres pelos"
    print(s"Lista de frecuencias de la lista de arriba ${lista}\n")
    print(s"Lista de frecuencias ordenada y pasado a hojas ${DistribFrecAListaHojas(lista)}\n")
    val miArbol= ArbolHuffman("texto para construir el arbol huffman")
    print(s"Arbol construido en la linea de arriba ${miArbol}\n")
    print(s"prueba de pasar de arbol a tabla ${deArbolATabla(miArbol)}\n")
    print(s"prueba del metodo codificarCurri ${codificarCurri(deArbolATabla(miArbol))("exto patruir le")}\n")
    print(s"prueba del metodo decodificarCurri (debe imprimir exto patruir le) ${decodificarCurri(deArbolATabla(miArbol))(listabits2  )}")
  }


}

object ArbolHuffman {
  def apply(cadena: String): ArbolHuffman = crearArbolHuffman(cadena)
}