define build_pdf
	xelatex -halt-on-error -output-directory _build $(1).tex
	mv _build/$(1).pdf .
endef


build:
	#(call build_pdf,02_02_beam_features)
	#(call build_pdf,02_02_beam_features)
	#(call build_pdf,02_03_elixir_features)
	$(call build_pdf,02_04_drawbacks)

clean:
	rm _build/*

show:
	# evince 02_01_history.pdf &
	# evince 02_02_beam_features.pdf &
	# evince 02_03_elixir_features.pdf &
	evince 02_04_drawbacks.pdf &
