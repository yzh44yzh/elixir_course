build:
	-mkdir _build
	xelatex -halt-on-error -output-directory _build 07_07_fp.tex
	mv _build/07_07_fp.pdf .

clean:
	rm _build/*

show:
	evince 07_07_fp.pdf &
