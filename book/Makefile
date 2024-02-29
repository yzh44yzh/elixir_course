build_pdf:
	-mkdir _build
	xelatex -halt-on-error -output-directory=_build book.tex

show:
	evince _build/book.pdf &

clean:
	rm -f _build/*
