build:
	@erlc -o ebin src/*.erl

run:
	@erl -pa ebin

join:
	@cd ebin; escript dq.beam
