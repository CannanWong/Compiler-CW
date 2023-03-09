all: 
	sbt compile assembly

fe_test:
	sbt compile assembly
	bash src/test/test_script/fe_test.sh

be_test:
	sbt compile assembly
	bash src/test/test_script/be_test.sh

be_all_test:
	sbt compile assembly
	bash src/test/test_script/be_all_test.sh

be_fail_test:
	sbt compile assembly
	bash src/test/test_script/be_fail_test.sh

ext_pair_test:
	sbt compile assembly
	bash src/test/test_script/ext_pair_test.sh

ext_fo_test:
	sbt compile assembly
	bash src/test/test_script/ext_fo_test.sh

ext_ph_test:
	sbt compile assembly
	bash src/test/test_script/ext_ph_test.sh

ext_ra_test:
	sbt compile assembly
	bash src/test/test_script/ext_ra_test.sh

clean:
	sbt clean && rm -rf wacc-45-compiler.jar && rm -f *.s

.PHONY: all clean