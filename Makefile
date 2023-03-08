all: 
	sbt compile assembly

fe_test:
	sbt compile assembly
	bash fe_test.sh

be_test:
	sbt compile assembly
	bash be_test.sh

be_all_test:
	sbt compile assembly
	bash be_all_test.sh

be_fail_test:
	sbt compile assembly
	bash be_fail_test.sh

ext_pair_test:
	sbt compile assembly
	bash ext_pair_test.sh

ext_fo_test:
	sbt compile assembly
	bash ext_fo_test.sh

ext_ph_test:
	sbt compile assembly
	bash ext_ph_test.sh

ext_ra_test:
	sbt compile assembly
	bash ext_ra_test.sh

clean:
	sbt clean && rm -rf wacc-45-compiler.jar && rm -f *.s

.PHONY: all clean