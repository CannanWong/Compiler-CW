all: 
	sbt compile assembly

test:
	sbt compile assembly
	bash test.sh

clean:
	sbt clean && rm -rf wacc-45-compiler.jar

.PHONY: all clean