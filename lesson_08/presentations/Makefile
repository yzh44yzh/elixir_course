build:
	-mkdir _build
	xelatex -halt-on-error -output-directory _build 08_04_let_it_crash.tex
	mv _build/08_04_let_it_crash.pdf .

clean:
	rm _build/*

show:
	evince 08_04_let_it_crash.pdf &
