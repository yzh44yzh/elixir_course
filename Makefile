build_pdf:
	xelatex -halt-on-error \
	book.tex

show:
	evince book.pdf &

# build_epub:
	# pandoc -o _build/elixir_course_junior.epub book.md
