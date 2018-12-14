build:
	@erlc -o ebin src/*.erl

run:
	@erl -pa ebin
