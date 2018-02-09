How to run:

1. add tinyir.jar to the folder lib/
2. add to src/main/resources/train the zip with the training files named train.zip
3. add to src/main/resources/test the zip with the testing files named test.zip
4. add to src/main/resources/validation the zip with the testing files named validation.zip
5. add to src/main/resources/test the *.txt with the codes
6. in the build.sbt uncomment the line corresponding to the algorithm that you want to run, should be something like this:
	mainClass in (Compile,run) := Some("src.main.scala.$”)
7. run on the command line:
	$ sbt run