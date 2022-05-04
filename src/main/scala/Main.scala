import java.util.concurrent.LinkedBlockingQueue
import scala.collection.immutable.ListMap
import scala.io.Source

object Main {

  val word_space = new LinkedBlockingQueue[String]()
  val freq_space = new LinkedBlockingQueue[Map[String, Int]]()

  private def get_words_from_txt(): Unit = {
    val stopWords = Source.fromResource("stop_words.txt").getLines.toSet

    val lines = Source.fromResource("frankenstein.txt").getLines
    var words = lines.flatMap(line => line.split(" "))
    words = words.map(w => w.toLowerCase.replaceAll("[^a-zA-Z]", ""))
    words = words.filter(w => !stopWords.contains(w) && w.length > 2)
    for (word <- words) {
      word_space.put(word)
    }
  }

  private def use_workers_to_process_words(): Unit = {
    val workers = for (_ <- 1 to 5) yield new Thread(new ProcessWords[String](word_space, freq_space))
    for (worker <- workers) worker.start()
    for (worker <- workers) worker.join()
  }

  private def merge_word_freqs = {
    var word_freqs: Map[String, Int] = Map[String, Int]().withDefaultValue(0)

    while (!freq_space.isEmpty) {
      for ((word, freq) <- freq_space.take()) {
        word_freqs += (word -> (freq + word_freqs(word)))
      }
    }
    word_freqs
  }

  def print_results(word_freqs:Map[String,Int]): Unit = {
    val top_words = ListMap(word_freqs.toSeq.sortWith(_._2 > _._2): _*).toList
    for ((w, c) <- top_words.take(20)) {
      println(s"$w - $c")
    }
  }

  def main(args:Array[String]): Unit = {
    get_words_from_txt()
    use_workers_to_process_words()
    val word_freqs = merge_word_freqs
    print_results(word_freqs)
  }
}
