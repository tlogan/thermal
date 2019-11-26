all: bin/meka

bin/meka: code-mlton/meka/chars-lang/chars.lex.sml code-mlton/meka/tokens-lang/tokens.yacc.sml
	mkdir -p bin; mlton -output bin/meka code-mlton/meka/config.mlb

code-mlton/meka/chars-lang/chars.lex.sml:
	mllex code-mlton/meka/chars-lang/chars.lex

code-mlton/meka/tokens-lang/tokens.yacc.sig code-mlton/meka/tokens-lang/tokens.yacc.sml:
	mlyacc code-mlton/meka/tokens-lang/tokens.yacc

clean:
	git clean -Xf; rm -r bin/*
