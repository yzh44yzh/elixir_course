render_templates:
	python tools/render.py

build_pdf:
	pandoc --highlight=tango -o _build/elixir_course_junior.pdf _build/root.md

show_pdf:
	evince _build/elixir_course_junior.pdf &

build_epub:
	pandoc -o _build/elixir_course_junior.epub _build/root.md
