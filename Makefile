COMPILER = ocamlc
PACKAGES = -linkpkg -package yojson -package str -package unix -package extlib
BASE_DIR = base
CLIENT_DIR = client
SERVER_DIR = server
BASE_SOURCES = $(BASE_DIR)/gameinfo.ml $(BASE_DIR)/json.ml $(BASE_DIR)/network.ml
CLIENT_SOURCES =  $(CLIENT_DIR)/destinations.ml $(CLIENT_DIR)/main.ml
SERVER_SOURCES = $(SERVER_DIR)/main.ml
BUILD = ocamlfind $(COMPILER) -thread $(PACKAGES) -I $(BASE_DIR) -I $(CLIENT_DIR)
CLIENT_EXECUTABLE = send_gameinfo
SERVER_EXECUTABLE = get_gameinfo

all: client_release server_release

client_release:
	$(BUILD) -o $(CLIENT_EXECUTABLE) $(BASE_SOURCES) $(CLIENT_SOURCES)

client_debug:
	$(BUILD) -g -o $(CLIENT_EXECUTABLE) $(BASE_SOURCES) $(CLIENT_SOURCES)

server_release:
	$(BUILD) -o $(SERVER_EXECUTABLE) $(BASE_SOURCES) $(SERVER_SOURCES)

server_debug:
	$(BUILD) -g -o $(SERVER_EXECUTABLE) $(BASE_SOURCES) $(SERVER_SOURCES)

clean:
	rm -f $(CLIENT_EXECUTABLE) $(SERVER_EXECUTABLE) **/*.cmi **/*.cmo **/*.cmx **/*.o
