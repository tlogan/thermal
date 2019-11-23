all: bin/ptltl

bin/ptltl: code/ptltl/chars-lang/chars.lex.sml code/ptltl/tokens-lang/tokens.yacc.sml
	mkdir -p bin; mlton -output bin/ptltl code/ptltl/config.mlb

code/ptltl/chars-lang/chars.lex.sml:
	mllex code/ptltl/chars-lang/chars.lex

code/ptltl/tokens-lang/tokens.yacc.sig code/ptltl/tokens-lang/tokens.yacc.sml:
	mlyacc code/ptltl/tokens-lang/tokens.yacc

clean:
	git clean -Xf; rm -r bin/*
