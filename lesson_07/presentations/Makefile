build:
	-mkdir _build
	xelatex -halt-on-error -output-directory _build 07_06_protocol.tex
	mv _build/07_06_protocol.pdf .

clean:
	rm _build/*

show:
	evince 07_06_protocol.pdf &
