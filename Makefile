COMPILER = ocamlfind ocamlc -thread
PACKAGES = -linkpkg -package yojson -package str -package unix -package extlib
CLIENT_SOURCES = gameinfo.ml json.ml network.ml destinations.ml main.ml
CLIENT_EXECUTABLE = send_gameinfo

all: client

client:
	$(COMPILER) $(PACKAGES) -o $(CLIENT_EXECUTABLE) $(CLIENT_SOURCES)

debug:
	$(COMPILER) -g $(PACKAGES) -o $(CLIENT_EXECUTABLE) $(CLIENT_SOURCES)

clean:
	rm -f $(CLIENT_EXECUTABLE) *.cmi *.cmo *.cmx *.o
