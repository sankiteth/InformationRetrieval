package src.main.scala

import ch.ethz.dal.tinyir.io.ReutersRCVStream

import java.io._

import scala.collection.mutable.ListBuffer
import scala.io.Source


object Validation{
  /* CHANGE THIS IF NEEDED */
  val pathValidation        = "src/main/resources/validation/"
  val filenameInput         = "results_validation.txt"
  val filenameOutput        = "f1_scores.txt"

  /* NO NEED TO CHANGE THIS */
  val reutersValidation     = new ReutersRCVStream(pathValidation)
  val validationDocs        = reutersValidation.stream

  def main(args: Array[String]) : Unit = {
    var codesInDoc = scala.collection.mutable.Map[Int, Set[String]]()
    for(doc <- validationDocs){
      codesInDoc(doc.ID) = doc.codes
    }

    var ScoresF1  = ListBuffer[Double]()
    val pw        = new PrintWriter(new File(filenameOutput))

    for (line <- Source.fromFile(filenameInput).getLines()) {
      var content         = line.split(" ")
      var DocID           = content.head.toInt
      var PredictedDocSet = content.tail.toSet
      var ActualDocSet    = codesInDoc(DocID)

      var truePositives   = PredictedDocSet.intersect(ActualDocSet)
      var falsePositives  = PredictedDocSet -- ActualDocSet
      var falseNegatives  = ActualDocSet -- PredictedDocSet

      var numTP = truePositives.size
      var numFP = falsePositives.size
      var numFN = falseNegatives.size

      var precision : Double = 0.0
      var recall    : Double = 0.0
      var F1        : Double = 0.0

      if((numTP + numFP)!=0){
        precision = numTP.toDouble / (numTP + numFP)
      }else{
        precision = 0.0
      }
      if((numTP + numFN)!=0){
        recall = numTP.toDouble / (numTP + numFN)
      }else{
        recall = 0.0
      }

      if((precision + recall)!=0){
        F1 = (2*precision*recall)/(precision + recall)
      }else{
        F1 = 0.0
      }
      ScoresF1 += F1

      println(s"${DocID} \tF1=$F1 \tP=$precision \tR=$recall")
      pw.write(s"${DocID} \tF1=$F1\n")
    }
    val averageF1s = ScoresF1.view.sum / ScoresF1.length
    println(s"-------------------------F1 Average = $averageF1s------------------------------")
    pw.write(s"\nAverage of all documents' F1 scores = $averageF1s")
    pw.close()
  }
}
