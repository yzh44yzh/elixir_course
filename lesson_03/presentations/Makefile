define build_pdf
	xelatex -halt-on-error -output-directory _build $(1).tex
	mv _build/$(1).pdf .
endef


build:
	$(call build_pdf,03_00_intro)

clean:
	rm _build/*

show:
	evince 03_00_intro.pdf &
