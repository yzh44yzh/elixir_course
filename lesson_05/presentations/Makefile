define build_pdf
	xelatex -halt-on-error -output-directory _build $(1).tex
	mv _build/$(1).pdf .
endef


build:
	$(call build_pdf,05_01_immutability)

clean:
	rm _build/*

show:
	evince 05_01_immutability.pdf &
