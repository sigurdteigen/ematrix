compile:
	./rebar compile

clean:
	./rebar clean

shell:
	cd ebin && erl

devclean: clean
	git clean -f