build_pdf:
	xelatex -halt-on-error \
	book.tex

show:
	evince book.pdf &

clean:
	rm book.aux book.log book.toc book.pdf book.out
	rm lesson_01/*.aux
	rm lesson_02/*.aux
	rm lesson_03/*.aux

# build_epub:
	# pandoc -o _build/elixir_course_junior.epub book.md
