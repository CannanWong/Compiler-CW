all: sbt compile assembly

test:
	sbt compile assembly
	for file in src/test/scala/wacc/*.wacc; do ./compile $$file; done

clean:
	sbt clean && rm -rf wacc-45-compiler.jar

.PHONY: all clean