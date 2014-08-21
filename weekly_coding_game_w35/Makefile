compile: 
	@erlc +debug_info -o ebin/ src/refactor_me.erl src/main.erl test/refactor_me_tests.erl

test: compile
	@cp src/refactor_me.erl ebin/
	@erl -pa ebin/ -sasl errlog_type error -eval 'main:run_tests().' | tee res
	@rm ebin/refactor_me.erl

verify: clean test
	@grep -q 'COVERAGE:1.0 ALLPASSING:true' res && echo 'SUCCESS' || echo 'FAIL'
	@rm res

clean:
	@find . -name \*.beam -delete
	@find . -name \*~ -delete