package src.main.scala

import ch.ethz.dal.tinyir.io._
import ch.ethz.dal.tinyir.processing.ReutersRCVParse
import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.processing
import scala.collection.mutable.ListBuffer
import scala.collection.Map
import breeze.linalg._
import breeze.numerics._
import collection.mutable.HashMap
import collection.mutable.ArrayBuffer
import scala.util.control.Breaks._
import java.io._
import scala.collection.mutable.SortedSet
import ch.ethz.dal.tinyir.processing.StopWords
import com.github.aztek.porterstemmer.PorterStemmer
import util.Random.shuffle

object SVM {
  def main(args: Array[String]): Unit = {

    val path: String = "result_svm_test.txt"
    val pathTrain   = "src/main/resources/train/"
    val pathTest    = "src/main/resources/test/"

    //Select K distinct most occuring words per class
    val K : Int = 100

    println("K=" + K)

    //The files must be in .zip format
    val reutersTrain: ReutersRCVStream
                      = new ReutersRCVStream(pathTrain)
    val trainSamplesNum: Int = reutersTrain.length

    println("Number of train files = " + trainSamplesNum)

    //Distinct codes in training set
    var disCodes : Set[String] = Set.empty

    //Code labels in training set
    var trainCodeLabels: Array[scala.collection.immutable.Set[String]]
                          = new Array[scala.collection.immutable.Set[String]](trainSamplesNum)

    //Document frequency of words in each class
    var classDf : scala.collection.mutable.Map[String,scala.collection.mutable.Map[String,Double]]
                      = scala.collection.mutable.Map.empty

    println("Code counting")
    var index: Int = -1
      for (doc <- reutersTrain.stream) {
        index = index + 1
        if (index == trainSamplesNum) {
          break
        }

        //Get labels of each doc
        disCodes ++= doc.codes
        trainCodeLabels(index) = doc.codes

        var docWordsMap: scala.collection.immutable.Map[String, Double] = tf(Tokenizer.tokenize(doc.content))

        //For each word in the doc, increase document frequency within the class by one
        for(c<-doc.codes){
          if(!classDf.contains(c))
            classDf += (c->scala.collection.mutable.Map.empty)
          for(t<-docWordsMap){
            if(!classDf(c).contains(t._1)){
              classDf(c) += (t._1->1.0)
            }
            else{
              classDf(c)(t._1) = classDf(c)(t._1) + 1.0
            }
          }
        }
      }

    val allCodes : Array[String] = disCodes.toArray
    disCodes = null
    val allCodesNum : Int = allCodes.size

    //Fast lookup for code to its index
    val allCodesMap : Map[String,Int] = allCodes.zipWithIndex.toMap

    println("Total uniue codes = " + allCodesNum)

    //Term frequencies of docs in training set
    var trainDocsTfs: Array[scala.collection.immutable.Map[String, Double]]
                      = new Array[scala.collection.immutable.Map[String, Double]](trainSamplesNum)

    //Term frequency of words in each class
    var classDocFreq : scala.collection.mutable.Map[String,scala.collection.mutable.Map[String,Double]]
                      = scala.collection.mutable.Map.empty

    println("Calculating term and collection frequencies")
    index = -1

      for (doc <- reutersTrain.stream) {
        index = index + 1
        if (index == trainSamplesNum) {
          break
        }

        //Get term frequency of words in this document
        var docWordsMap: scala.collection.immutable.Map[String, Double] = tf(Tokenizer.tokenize(doc.content))
        trainDocsTfs(index) = docWordsMap

        //Calculate term frequencies within each class
        for(c<-doc.codes){
          if(!classDocFreq.contains(c))
            classDocFreq += (c-> scala.collection.mutable.Map.empty)
          for(t<-docWordsMap){
            classDocFreq(c) +=
              classDocFreq(c).get(t._1).map(x => t._1 -> (x + t._2)).getOrElse(t._1 -> t._2)
          }
        }
      }

    println("Feature selection")
    //FREQUENCY BASED FEATURE SELECTION
    //TAKE MOST FREQUENT 'K' WORDS FROM EACH CLASS, AND THEN THEIR UNION

    println("Calculate inverse document frequency within class")
    var temp = classDocFreq
    //Calculate document frequency within class
    for(i<-classDocFreq){
      var a = classDf.values
      for(j<-i._2){
        var b = a.filter(word=>(word==j._1))
        var sum = 0.0
        for(k<-b){
          sum += k.values.sum
        }
        temp(i._1)(j._1) = temp(i._1)(j._1)/sum
      }
    }

    classDocFreq = temp

    var feature: Set[String] = Set.empty
    for(i<-0 to allCodesNum-1){
      feature = feature ++
        classDocFreq.getOrElse(allCodes(i), Map.empty).toSeq.sortBy(-_._2).take(K).map(_._1).toSet
    }

    classDocFreq = null

    val featureNum : Int = feature.size
    println("Feature size = " + featureNum)

    println("Recalculate tf:")

    //Re-calculate term frequencies based on features selected
    for(i<-0 to trainSamplesNum-1){
      trainDocsTfs(i) = trainDocsTfs(i).filter(p=>feature.contains(p._1))
      trainDocsTfs(i) = trainDocsTfs(i).mapValues { x => x/trainDocsTfs(i).size }
    }

    //Map to calculate feature vector for each doc
    val zeroFeatureVector: Map[String, Double] = feature.groupBy(identity).mapValues(x => 0.0)

    //##############################SVM#########################
    //-----------------------------Train--------------------------------------

    var eta: Double = 50.0
    var lambda: Double = 0.000001
    var theta: DenseMatrix[Double] = DenseMatrix.zeros[Double](featureNum, allCodesNum)

    println("Training start:")

    var trainDesignVec: DenseVector[Double] = DenseVector.zeros[Double](featureNum)
    //Iterate over feature vector of each training data, pick data randomly
    //Stochastic gradient descent on training samples
    index = 0
    for (i <- shuffle(0 to trainSamplesNum - 1)) {
      index += 1
      if(index%1000 == 0){
        println(index)
      }

      //Feature vector for this doc
      trainDesignVec = new DenseVector((zeroFeatureVector ++ trainDocsTfs(i)).toSeq.sortBy(_._1).map(_._2).toArray)

      //Update parameters of all classifiers, both positive and negative
      theta = theta * (1-(eta*lambda))

      //Label of document
      var y: Int = 0
      //Train one classifier for each code in all codes
      for (j <- 0 to allCodesNum - 1) {
        if (trainCodeLabels(i).contains(allCodes(j))) {
          y = 1
        } else {
          y = -1
        }

        //Parameter update only for negative examplesS
        if ((trainDesignVec dot theta(::, j)) * y < 1) {
          theta(::,j) += (trainDesignVec * (eta*y))
        }
      }
    }

    //-----------------------------------TESTING------------------------------------

    val reutersTest: ReutersRCVStream
                    = new ReutersRCVStream(pathTest)
    val testSamplesNum: Int = reutersTest.length

    println("Number of test files = " + testSamplesNum)

    var testDocsTfs: Array[scala.collection.immutable.Map[String, Double]]
                    = new Array[scala.collection.immutable.Map[String, Double]](testSamplesNum)

    var prediction: Array[Set[String]] = new Array[Set[String]](testSamplesNum)

    var docIds = ListBuffer[Int]()

    //Initialize prediction
    for (i <- 0 to testSamplesNum - 1) {
      prediction(i) = Set.empty
    }

    index = -1

      for (doc <- reutersTest.stream) {
        index = index + 1
        docIds += doc.ID
        //Calculate TF score for each doc
        testDocsTfs(index) = tf(Tokenizer.tokenize(doc.content))

      }

    println("Recalculate tf based on features selected:")
    //Re-calculate tfs based on features selected
    for(i<-0 to testSamplesNum-1){
      testDocsTfs(i) = testDocsTfs(i).filter(p=>feature.contains(p._1))
      testDocsTfs(i) = testDocsTfs(i).mapValues { x => x/testDocsTfs(i).size }
    }

    println("Prediction start")
    //Consider only those words that are present in the training vocabulary
    for (i <- 0 to testSamplesNum - 1) {
      //println(i)
      var testFeaVec: DenseVector[Double]
                = DenseVector((zeroFeatureVector ++ testDocsTfs(i)).toSeq.sortBy(_._1).map(_._2).toArray)

      for (j <- 0 to allCodesNum - 1) {
        var mult = theta(::, j) dot testFeaVec
        if (mult > 0) {
          prediction(i) += allCodes(j)
        }

      }
    }

    println("Prediction end")
    var output : String = ""
    for (i <- 0 to testSamplesNum - 1) {
      output += docIds(i) + " " + prediction(i).mkString(" ") + "\r\n"
    }

    writeToFile(output, path)

  }

  def tf(doc: List[String]): scala.collection.immutable.Map[String, Double] = {
    var filteredDoc: Seq[String] = StopWords.filterOutSW(doc)

    filteredDoc.map(word => PorterStemmer.stem(word)).groupBy(identity).mapValues(_.size)
  }

  def writeToFile(output: String, path: String) {
    val fw = new FileWriter(path, true)
    try {
      fw.write("\r\n" + output + "\r\n")
    } finally fw.close()
  }
}
