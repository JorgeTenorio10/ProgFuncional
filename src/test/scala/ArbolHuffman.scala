import scala.annotation.tailrec

type Bit = 0 | 1
type TablaCodigos = List[(Char,List[Bit])]

def deArbolATabla(arbol:ArbolHuffman):TablaCodigos=
  def deArbolATablaAux(arbol:ArbolHuffman,camino: List[Bit]): TablaCodigos= arbol match
    case HojaHuff(caracter,_)=>List((caracter,camino))
    case RamaHuff(izq,dcho)=> deArbolATablaAux(izq,camino:+0)++ deArbolATablaAux(dcho,camino:+1)
  deArbolATablaAux(arbol,List())

  def codificarCurri(tabla: TablaCodigos)(cadena: String): List[Bit] =
    def buscarCodigo(caracter: Char, tabla: TablaCodigos): List[Bit] = tabla match
      case (c, bits) :: _ if c == caracter => bits
      case _ :: resto => buscarCodigo(caracter, resto)
      case Nil => throw new IllegalArgumentException(s"'$caracter' no se encuentra en la tabla")


    // Para cada carácter obtenemos el codigo y lo añadimos a la lista de bits
    def codificarCurriAux(c: List[Char], res: List[Bit]): List[Bit] = c match
      case Nil => res
      case c :: resto => val codigo = buscarCodigo(c, tabla)
        codificarCurriAux(resto, res ++ codigo)

    codificarCurriAux(cadena.toList, List())


  def decodificarCurri(tabla: TablaCodigos)(bits: List[Bit]): String =
    // Función auxiliar para buscar el carácter que corresponde a la secuencia de bits inicial
    def buscarCaracter(bits: List[Bit], tabla: TablaCodigos): (Char, List[Bit]) = tabla match
      case (c, codigo) :: _ if empiezaCon(bits, codigo) => (c, bits.drop(codigo.length))
      case _ :: resto => buscarCaracter(bits, resto)
      case Nil => throw new IllegalArgumentException("Este código no está en la tabla")

    // Función auxiliar para comparar si una lista de bits comienza con otra
    def empiezaCon(bits: List[Bit], prefijo: List[Bit]): Boolean =
      (bits, prefijo) match
        case (_, Nil) => true
        case (h :: t, he :: ta) if h == he => empiezaCon(t, ta)
        case _ => false

    // Decodifica recursivamente bits, construyendo la lista de caracteres decodificados
    def decodificarCurriAux(bitsRestantes: List[Bit], resultado: List[Char]): List[Char] =
      if (bitsRestantes.isEmpty) resultado
      else
        val (caracter, nuevosBitsRestantes) = buscarCaracter(bitsRestantes, tabla)
        decodificarCurriAux(nuevosBitsRestantes, resultado :+ caracter)

    // Convertimos la lista de caracteres en una cadena final y la retornamos
    decodificarCurriAux(bits, List()).mkString


def cadenaAListaChars(cadena:String):List[Char]=
  cadena.toList

def listaCharsACadena(caracteres:List[Char]):String=
  caracteres.mkString

def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] = {
  @tailrec
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

def DistribFrecAListaHojas(frec:List[(Char,Int)]):List[HojaHuff]=
  @tailrec
  def DistribFrecAListaHojasAux(frec:List[(Char,Int)], res:List[HojaHuff]):List[HojaHuff]= frec match
    case Nil=> val hojasOrdenadas = res.sortBy(_.peso)
                hojasOrdenadas
    case (cabeza,peso)::resto=> val hojas=HojaHuff(cabeza,peso)::res
      DistribFrecAListaHojasAux(resto,hojas)

  DistribFrecAListaHojasAux(frec,Nil)

def crearRamaHuff(izq: ArbolHuffman, dch: ArbolHuffman): RamaHuff =
  RamaHuff(izq, dch)

def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = nodos match
  case primero :: segundo :: resto =>
    val nuevaRama = crearRamaHuff(primero, segundo)
    val listaActualizada = (nuevaRama :: resto).sortBy(nodo => nodo.peso(nodo))
    listaActualizada
  case _ => nodos

def esListaSingleton(lista: List[ArbolHuffman]): Boolean =
  lista.length == 1

@tailrec
def repetirHasta(combinar: List[ArbolHuffman] => List[ArbolHuffman], esListaSingleton: List[ArbolHuffman] => Boolean)(lista: List[ArbolHuffman]): List[ArbolHuffman] =
  if (esListaSingleton(lista)) lista
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

  


abstract class ArbolHuffman {
  def peso(arbol: ArbolHuffman): Int = arbol match
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
    case _ if !contieneCaracter(caracter) => throw new IllegalArgumentException("Carácter no encontrado en el árbol")


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

object ArbolHuffman {
  def apply(cadena: String): ArbolHuffman = crearArbolHuffman(cadena)
}