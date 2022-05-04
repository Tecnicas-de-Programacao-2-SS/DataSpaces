import java.util.concurrent.LinkedBlockingQueue
import scala.util.control.Breaks._

class ProcessWords[T](word_space: LinkedBlockingQueue[String], freq_space: LinkedBlockingQueue[Map[String, Int]]) extends Runnable {
  def run(): Unit = {
    var word_freqs: Map[String, Int] = Map().withDefaultValue(0)
    breakable {
      while (true) {
        val word = word_space.poll()
        if (word == null) break
        word_freqs += (word -> (word_freqs(word) + 1))
      }
    }
    freq_space.put(word_freqs)
  }
}

