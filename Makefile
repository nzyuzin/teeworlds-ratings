COMPILER = ocamlfind ocamlc
PACKAGES = -linkpkg -package yojson -package str -package unix
CLIENT_SOURCES = gameinfo.ml json.ml main.ml
CLIENT_EXECUTABLE = send_gameinfo

all: client

client:
	$(COMPILER) $(PACKAGES) -o $(CLIENT_EXECUTABLE) $(CLIENT_SOURCES)

debug:
	$(COMPILER) -g $(PACKAGES) -o $(CLIENT_EXECUTABLE) $(CLIENT_SOURCES)

clean:
	rm -f $(CLIENT_EXECUTABLE) *.cmi *.cmo *.cmx *.o
