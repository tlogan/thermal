all: bin/thermal

bin/thermal: impl/chars-lang/chars.lex.sml impl/tokens-lang/tokens.yacc.sml
	mkdir -p bin; mlton -output bin/thermal impl/config.mlb

impl/chars-lang/chars.lex.sml:
	mllex impl/chars-lang/chars.lex

impl/tokens-lang/tokens.yacc.sig impl/tokens-lang/tokens.yacc.sml:
	mlyacc impl/tokens-lang/tokens.yacc

clean:
	git clean -Xf; rm -r bin/*
