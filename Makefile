COMPILER = ocamlfind ocamlc -linkpkg -package yojson -package str -package unix
CLIENT_SOURCES = send_score.ml
CLIENT_EXECUTABLE = report_scores

all: client

client:
	$(COMPILER) -o $(CLIENT_EXECUTABLE) $(CLIENT_SOURCES)

clean:
	rm -f $(CLIENT_EXECUTABLE) *.cmi *.cmo *.cmx *.o
