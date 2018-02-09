package src.main.scala

import breeze.linalg._
import breeze.numerics._
import breeze.math._

import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.ReutersRCVParse
import ch.ethz.dal.tinyir.processing.Tokenizer.tokenize
import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.processing.Document
import ch.ethz.dal.tinyir.processing.StopWords.stopWords
import ch.ethz.dal.tinyir.lectures.TermFrequencies
import ch.ethz.dal.tinyir.lectures.TermFrequencies.log2
import ch.ethz.dal.tinyir.util.StopWatch

import com.github.aztek.porterstemmer.PorterStemmer

import java.io._

import scala.collection.mutable.ListBuffer
import scala.collection.Map


object NaiveBayes{

  val pathTrain   = "src/main/resources/train/"
  val pathTest    = "src/main/resources/test/"
  val reuters     = new ReutersRCVStream(pathTrain)
  val reutersTest = new ReutersRCVStream(pathTest)

  val numDocs       = 50000
  val numDocsTest   = 10000
  val numDocsTrain  = numDocs

  def trainDocs   = reuters.stream
  def testDocs    = reutersTest.stream

  val codes = readAllCodes() // O(docs)
  val tf    = getTfs()  // O(docs*docWords)

  val vocabulary      = getVocabulary() // O(tf)
  val vocabularySize  = vocabulary.size
  val testVocabulary  = getTestVocabulary() // O(testDocs)

  val sums  = getSums() // O(docs*docCodes*docWords)

  // calculating the sum of the lengths of all documents in the training set
  var totalDocsSize :Long = 0
  for(doc <- trainDocs){
    def tokens = doc.tokens.filter(!stopWords.contains(_)).map(w => PorterStemmer.stem(w))
    totalDocsSize += tokens.length
  }

  val lenDocsClass = MapLenDocsClass() // O(docs*docCodes)

  // the sum of the length of the documents not in a given class is the same as
  // the total length of all documents minus the sum of the length of the documents
  // in a given class
  var lenDocsNotClass = lenDocsClass.map{case(k,v) => k -> (totalDocsSize - v)}

  def main(args: Array[String]) : Unit = {
    val sw_global = new StopWatch; sw_global.start

    val pw = new PrintWriter(new File("results_testset.txt"))

    var k = 0
    val (logPcodes, logPNotCodes) = MaplogPcodes()

    for(testDoc <- testDocs){
      var documentName = testDoc.title
      pw.write(s"${testDoc.ID}")

      // get term frequencies for given testDoc
      def tokens = tokenize(testDoc.content).filter(!stopWords.contains(_)).map(w => PorterStemmer.stem(w))
      val tfDoc = TermFrequencies.tf(tokens)

      // for each code we want to check if P(c|d) > P(not-c|d)
      for(code <- codes){
        val logPcode = logPcodes(code)
        val logPnotCode = logPNotCodes(code)

        if(logPnotCode!=0){ // logP(not-c)=0 means that no document has the code c

          val logProbDC  = logProbDocGivenCode(testDoc, code) // O(testDocWords*O(1))
          val logProbNotC = logProbDocGivenNotCode(testDoc, code)

          // println(s"logProbDC, logProbNotC : $logProbDC $logProbNotC")
          val pCodegivenDoc = logPcode + logProbDC
          val pNotCodegivenDoc = logPnotCode + logProbNotC

          // println(pCodegivenDoc, pNotCodegivenDoc)

          if( pCodegivenDoc > pNotCodegivenDoc ){
            // println(s"------------------------> document $documentName has code $code")
            pw.write(s" ${code}")
          }

        }
      }

      pw.write(s"\n")
      k += 1
    }

    sw_global.stop
    // println("Running time : " + sw_global.stopped)
    pw.close()
  }

  def getTfs() : (Map[String, Int]) = {
    /* returns a map with the frequency of each word in all training documents */
    var tfs = scala.collection.mutable.Map[String, Int]()
    var docTf = Map[String, Int]()

    var index = 1
    for(doc <- trainDocs){
      // for all documents calculate its termfrequency and add to tfs
      var tokens = doc.tokens.filter(!stopWords.contains(_)).map(w => PorterStemmer.stem(w))
      docTf = TermFrequencies.tf(tokens)

      for((word, frequency) <- docTf){
        tfs(word) = (tfs.getOrElse(word,0) + frequency)
      }
    }
    tfs
  }

def getSums() : Map[(Int, String), Int]= {
  /* returns sum of the word frequency in all training documents that have a given code */
  val sw = new StopWatch; sw.start
  var sumTfPerCode = scala.collection.mutable.Map[(Int,String), Int]()
  var docTf = Map[String, Int]()
  var wordIndex : Int = 0
  // val docs = trainDocs
  var index : Int = 0

  for(doc <- trainDocs){
    def tokens = doc.tokens.filter(!stopWords.contains(_)).map(w => PorterStemmer.stem(w))
    docTf = TermFrequencies.tf(tokens)


    for(code <- doc.codes){
      for((word, frequency) <- docTf){
        // if word not in testVocabulary the sumFrequency of that word will never be needed
        // if word not in vocabulary the word never shows up in the trainDocs, hence the sum is zero
        if(testVocabulary.contains(word) && vocabulary.contains(word)){
          wordIndex = vocabulary(word)

          sumTfPerCode((wordIndex, code)) = sumTfPerCode.getOrElse(wordIndex->code, 0) + frequency
        }
      }
    }
    index += 1
  }
  sw.stop
  sumTfPerCode
}
  def MaplogPcodes() : (Map[String, Double],  Map[String, Double]) = {
    /* returns a map with logP(c) and another with logP(not-c) */
    var result = scala.collection.mutable.Map[String, Double]()

    for(doc <- trainDocs){
      for(code <- doc.codes){
        result(code) = (result.getOrElse(code, 0.0))
        result(code) += 1
      }
    }

    val finalLogPNotCodes = result.map{case(k,v) => k -> (log2(numDocsTrain - v) - log2(numDocsTrain))}
    val finalLogPCodes = result.map{case(k,v) => k -> (log2(v) - log2(numDocsTrain))}

    (finalLogPCodes, finalLogPNotCodes)
  }

 def getVocabulary() : Map[String, Int] = {
   /* return a map of all the words in the training documents and a corresponding index */
   tf.keys.toList.zipWithIndex.toMap
}

 def getTestVocabulary() : Map[String, Int] = {
  /* return a map of all the words in the testing documents and a corresponding index */
    var vocabulary = scala.collection.immutable.Set[String]()

    for(doc <- testDocs){
      def tokens = doc.tokens.filter(!stopWords.contains(_)).map(w => PorterStemmer.stem(w))
      tokens.foreach{token => vocabulary += token}
    }
    vocabulary.toList.zipWithIndex.toMap
}
  def MapLenDocsClass() : Map[String, Int] = {
    /* returns a map where the key is a code and the value is the sum of the
    / length of the documents with that given code */

    var result = scala.collection.mutable.Map[String, Int]()

    for(doc <- trainDocs){
      def tokens = doc.tokens.filter(!stopWords.contains(_)).map(w => PorterStemmer.stem(w))
      for(code <- doc.codes){
        result(code) = result.getOrElse(code, 0) + tokens.length
      }
    }
    result.toMap
  }

  def logProbWordGivenCode(word : String, code : String) : Double ={
    /* returns P(w|c) */
    var numeratorLaplace : Long = 1 + sums.getOrElse((vocabulary.getOrElse(word, -1), code), 0)
    var denominatorLaplace : Long = vocabularySize + lenDocsClass(code)

    log2(numeratorLaplace) - log2(denominatorLaplace)
  }

  def logProbWordGivenNotCode(word : String, code : String) : Double ={

    var numeratorLaplace : Long = 1 + tf.getOrElse(word, 0) - sums.getOrElse((vocabulary.getOrElse(word, -1), code), 0)
    var denominatorLaplace : Long = vocabularySize + lenDocsNotClass(code)

    log2(numeratorLaplace) - log2(denominatorLaplace)
  }

  def logProbDocGivenCode(doc : Document, code : String) : Double = {
    /* returns P(d|c) */

    var result : Double = 0
    val tokens = doc.tokens.filter(!stopWords.contains(_)).map(w => PorterStemmer.stem(w))

    for(word <- tokens){
      // since it's not a set there's no need to multiply by the term frequency
      result += logProbWordGivenCode(word, code)
    }
    result
  }

  def logProbDocGivenNotCode(doc : Document, code : String) : Double = {
    var result : Double = 0
    val tokens = doc.tokens.filter(!stopWords.contains(_)).map(w => PorterStemmer.stem(w))

    for(word <- tokens){
      // since it's not a set there's no need to multiply by the term frequency
      result += logProbWordGivenNotCode(word, code)
    }
    result
  }

  def readAllCodes() : List[String] = {
    /* returns a list of the codes tht appear in the training documents */
    var codes = scala.collection.immutable.Set[String]()

    for(doc <- trainDocs){
      for(code <- doc.codes){
        codes += code
      }
    }
    codes.toList
  }
}
