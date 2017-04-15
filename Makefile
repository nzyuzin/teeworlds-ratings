COMPILER = ocamlc
PACKAGES = -linkpkg -package yojson -package str -package unix -package extlib
BASE_DIR = base
CLIENT_DIR = client
BASE_SOURCES = $(BASE_DIR)/gameinfo.ml $(BASE_DIR)/json.ml $(BASE_DIR)/network.ml
CLIENT_SOURCES =  $(CLIENT_DIR)/destinations.ml $(CLIENT_DIR)/main.ml
BUILD = ocamlfind $(COMPILER) -thread $(PACKAGES) -I $(BASE_DIR) -I $(CLIENT_DIR)
CLIENT_EXECUTABLE = send_gameinfo

all: client_release

client_release:
	$(BUILD) -o $(CLIENT_EXECUTABLE) $(BASE_SOURCES) $(CLIENT_SOURCES)

client_debug:
	$(BUILD) -g -o $(CLIENT_EXECUTABLE) $(BASE_SOURCES) $(CLIENT_SOURCES)

clean:
	rm -f $(CLIENT_EXECUTABLE) **/*.cmi **/*.cmo **/*.cmx **/*.o
