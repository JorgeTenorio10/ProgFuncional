trait CodigoHuffman{
  def peso(arbol: ArbolHuffman):Int= arbol match
    case HojaHuff(_, peso)=> peso
    case RamaHuff(nodoIzq,nodoDcho)=>peso(nodoIzq) + peso(nodoDcho)

  def caracteres(arbol:ArbolHuffman):List[Char]= arbol match
    case HojaHuff(caracter,_)=>List(caracter)
    case RamaHuff(nodoIzq,nodoDcho)=>caracteres(nodoIzq)++caracteres(nodoDcho)
}


