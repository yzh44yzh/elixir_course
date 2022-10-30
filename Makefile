build_pdf:
	xelatex -halt-on-error \
	book.tex

show_pdf:
	evince book.pdf &

# build_epub:
	# pandoc -o _build/elixir_course_junior.epub book.md
