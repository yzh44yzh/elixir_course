pdf:
	xelatex -halt-on-error -output-directory _build 02_01_history.tex
	mv _build/02_01_history.pdf .

clean:
	-rm *.bak
	rm _build/*

show:
	evince 02_01_history.pdf&
