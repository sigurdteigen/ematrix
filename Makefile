compile:
	./rebar compile

clean:
	./rebar clean

shell: compile
	cd ebin && erl

devclean: clean
	git clean -f