all: bin/thermal

bin/thermal: code-mlton/chars-lang/chars.lex.sml code-mlton/tokens-lang/tokens.yacc.sml
	mkdir -p bin; mlton -output bin/thermal code-mlton/config.mlb

code-mlton/chars-lang/chars.lex.sml:
	mllex code-mlton/chars-lang/chars.lex

code-mlton/tokens-lang/tokens.yacc.sig code-mlton/tokens-lang/tokens.yacc.sml:
	mlyacc code-mlton/tokens-lang/tokens.yacc

clean:
	git clean -Xf; rm -r bin/*
