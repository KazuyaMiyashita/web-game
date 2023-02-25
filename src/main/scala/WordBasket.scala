case class WordBasket(next: Char) {
  def accept(word: String): Either[Throwable, WordBasket] = {
    Either.cond(word.length == 3 && word.head == next, WordBasket(word.last), new Exception("illegal word"))
  }
}
object WordBasket {
  private val words = List(
    'あ', 'い', 'う', 'え', 'お', 'か', 'き', 'く', 'け', 'こ', 'さ', 'し', 'す', 'せ', 'そ', 'た', 'ち', 'つ', 'て', 'と', 'な', 'に', 'ぬ',
    'ね', 'の', 'は', 'ひ', 'ふ', 'へ', 'ほ', 'ま', 'み', 'む', 'め', 'も', 'や', 'ゆ', 'よ', 'ら', 'り', 'る', 'れ', 'ろ', 'わ', 'を', 'ん'
  )
  def create(): WordBasket = WordBasket(words(util.Random.nextInt(words.length)))
}
