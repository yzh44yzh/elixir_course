xelatex_pdf:
	cd _build && \
	xelatex -halt-on-error \
	-output-directory output \
	document.tex

pandoc_pdf:
	pandoc --highlight=tango -o _build/elixir_course_junior.pdf \
	book.md \
	lesson_01/01_01.fizz_buzz.ru.md \
	lesson_01/01_02.iex.ru.md \
	lesson_02/02_01_history.md

show_pdf:
	# evince _build/elixir_course_junior.pdf &
	evince _build/output/document.pdf &

build_epub:
	pandoc -o _build/elixir_course_junior.epub book.md

render_templates:
	python tools/render.py
