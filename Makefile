all: 
	sbt compile assembly

fe_test:
	sbt compile assembly
	bash fe_test.sh

be_test:
	sbt compile assembly
	bash be_test.sh

clean:
	sbt clean && rm -rf wacc-45-compiler.jar

.PHONY: all clean