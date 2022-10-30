xelatex_pdf:
	cd _build && \
	xelatex -halt-on-error \
	-output-directory output \
	document.tex

show_pdf:
	# evince _build/elixir_course_junior.pdf &
	evince _build/output/document.pdf &

build_epub:
	pandoc -o _build/elixir_course_junior.epub book.md
