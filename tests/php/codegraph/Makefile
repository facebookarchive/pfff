graph:
	~/pfff/codegraph -lang php -build .
	mv graph_code.marshall save_code.marshall
	~/pfff/codegraph -adjust_graph save_code.marshall adjust_graph.txt info_whitelist.txt graph_code.marshall
	cat pfff.log
