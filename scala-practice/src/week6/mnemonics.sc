import scala.io.Source

object mnenomic {

  val in  = Source.fromURL("https://en.wikipedia.org/wiki/Main_Page")

  val words  = in.getLines().toList filter( word => word forall(chr => chr.isLetter))

  val mnem = Map('2' -> "ABC",'3' -> "DEF",'4'-> "GHI",'5'-> "JKL",
    '6'->"MNO",'7'-> "PQRS",'8'->"TUV",'9'->"WXYZ")

  val charCode : Map[Char,Char]=for ((digit,str) <-mnem; ltr<- str) yield ltr -> digit


  //map a word to digit string

  def wordCode(word :String):String = word.toUpperCase map charCode


  wordCode("JAVA")

  //map from digit string to the words that represent them
  // eg "5282" -> List("Java","Kata","Lava"...)


  val wordsFromNum : Map[String,Seq[String]] = words groupBy wordCode withDefaultValue Seq()


  def encode(number :String) :Set[List[String]] =
    if(number.isEmpty) Set(List())
  else{
      for{
        split <- 1 to number.length
        word <- wordsFromNum(number take split )
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet


  def translate(number:String):Set[String] = encode(number) map (_ mkString " ")

}