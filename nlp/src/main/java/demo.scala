import java.io.{IOException, FileNotFoundException}
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.neural.rnn.RNNCoreAnnotations
import edu.stanford.nlp.sentiment.SentimentCoreAnnotations
import edu.stanford.nlp.pipeline._
import scala.io.Source._
import java.util.Properties
import scala.Console._
import scala.io.StdIn
// Needed for sentiment
import scala.collection.JavaConversions._


/**
  * Created by Kyle Bashford on 4/25/2015.
 */

object demo {

  def main(args: Array[String]) {

    println(Console.BOLD + "Welcome to Standford NLP Demo\nType \"man\" to view commands" + Console.RESET)

    while(true) {
      val input = StdIn.readLine("command >>> ")
      repl(input)
    }
  }

  def man(): Unit = {
    println("\nManual")
    println("======\n")
    println("[man:  print manual                                 ]")
    println("[quit: exit the program                             ]")
    println("[exit: exit the program                             ]")
    println("[read-sent: read in a file for sentiment evaluation ]")
    println("[sent: get sentiment of user input                  ]")
    println("[read-pos: parse text into part-of-speech           ]")
    println("[pos: parse user input into part-of-speech          ]")
    println()
  }

  def repl(input: String): Unit = input match {
    case "quit" | "exit" => println("Quitting..."); System.exit(1)
    case "man" => man()
    case "read-sent" => sentFile()
    case "sent" => sentiment(StdIn.readLine("Enter a sentence >>> "))
    case "read-pos" => posFile()
    case "pos" => pos(StdIn.readLine("Enter a sentence >>> "))
    case _ => println(Console.RED + "Error, Wrong Command\n" + Console.RESET)
  }

  def sentFile(): Unit = {
    val location = StdIn.readLine("Enter File Name >>> ")
      try {
        val file = fromFile(location)
        // C:\Users\Kyle\Dropbox\Private\COG376\nlp\src\main\java\finn.txt
        // /home/kgb/Dropbox/Private/COG376/nlp/src/main/java/finn.txt
        println(Console.RED + "=== Processing file: " + Console.BOLD + file.descr + Console.RESET + Console.RED + " ===" + Console.RESET)
        sentiment(file.mkString)
      } catch {
        case e: FileNotFoundException => println(Console.RED + "404 Error: File Not Found" + Console.RESET)
        case e: IOException => println(Console.RED + "Cannot read file" + Console.RESET)
      }
  }

  def sentiment(sentence: String): Unit = {
    // properties are optional, but take longer without defining them
    val properties = new Properties()

    // properties.setProperty("annotators", "tokenize, ssplit, pos, lemma, parse, sentiment")

    properties.setProperty("annotators", "tokenize, ssplit, parse, sentiment")
    val pipeline = new StanfordCoreNLP(properties)
    val annotations = new Annotation(sentence)
    pipeline.annotate(annotations)
    // pipeline.prettyPrint(annotations, System.out)

    // need java conversions for this list
    var totalSentiment = 0
    var sentenceCount = 0
    for (word <- annotations.get(classOf[SentencesAnnotation])) {
      val tree = word.get(classOf[SentimentCoreAnnotations.AnnotatedTree])
      val sentimentNum = RNNCoreAnnotations.getPredictedClass(tree)
      val sentimentValue = word.get(classOf[SentimentCoreAnnotations.ClassName])
      printSentiment(sentimentValue, word.toString, sentimentNum)
      sentenceCount = sentenceCount + 1
      totalSentiment += sentimentNum
    }

    val averageSentiment: Double = totalSentiment.asInstanceOf[Double] / sentenceCount
    println("Average sentiment: " + Console.YELLOW + averageSentiment + Console.RESET)

  }

  def printSentiment(value: String, sentence: String, number: Int): Unit = value match {
    case "Positive" => println("Sentence: " + sentence + "\nSentiment Analysis: " + Console.GREEN + value + Console.RESET + "\nSentiment Value: " + Console.GREEN + number + Console.RESET + "\n")
    case "Negative" => println("Sentence: " + sentence + "\nSentiment Analysis: " + Console.RED + value + Console.RESET + "\nSentiment Value: " + Console.RED + number + Console.RESET + "\n")
    case "Neutral" => println("Sentence: " + sentence + "\nSentiment Analysis: " + Console.CYAN + value + Console.RESET + "\nSentiment Value: " + Console.CYAN + number + Console.RESET + "\n")
    case _ => println(Console.RED + "[Error, with sentence: "+ sentence +"]\n + [Sentiment value: " + value + "]" + Console.RESET + "\n")
  }

  def posFile(): Unit = {
    val location = StdIn.readLine("Enter File Name >>> ")
    try {
      val file = fromFile(location)
      // C:\Users\Kyle\Dropbox\Private\COG376\nlp\src\main\java\finn.txt
      // /home/kgb/Dropbox/Private/COG376/nlp/src/main/java/finn.txt
      println(Console.RED + "=== Processing file: " + Console.BOLD + file.descr + Console.RESET + Console.RED + " ===" + Console.RESET)
      pos(file.mkString)
    } catch {
      case e: FileNotFoundException => println(Console.RED + "404 Error: File Not Found" + Console.RESET)
      case e: IOException => println(Console.RED + "Cannot read file" + Console.RESET)
    }
  }

  def pos(sentence: String): Unit = {
    val properties = new Properties()
    properties.setProperty("annotators", "tokenize, ssplit, pos, lemma, parse")
    val pipeline = new StanfordCoreNLP(properties)
    val annotations = new Annotation(sentence)
    pipeline.annotate(annotations)
    pipeline.prettyPrint(annotations, System.out)
  }

}
