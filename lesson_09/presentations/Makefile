build:
	-mkdir _build
	xelatex -halt-on-error -output-directory _build 09_08_what_is_monad.tex
	mv _build/09_08_what_is_monad.pdf .

clean:
	rm _build/*

show:
	evince 09_08_what_is_monad.pdf &
