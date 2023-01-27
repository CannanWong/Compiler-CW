all:
	sbt compile assembly

clean:
	sbt clean && rm -rf wacc-45-compiler.jar

.PHONY: all clean
