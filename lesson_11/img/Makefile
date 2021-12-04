# https://www.graphviz.org/

generate_images:
	dot -Tpng:cairo -o gen_server.custom.png gen_server.custom.dot
	dot -Tpng:cairo -o gen_server.standard.png gen_server.standard.dot

view_images:
	eog gen_server.custom.png &
	eog gen_server.standard.png &
