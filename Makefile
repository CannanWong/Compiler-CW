all:
	sbt compile assembly

test:
	for i in src/test/*.wacc; do ./compile src/test/$i.wacc; done

clean:
	sbt clean && rm -rf wacc-45-compiler.jar

.PHONY: all clean