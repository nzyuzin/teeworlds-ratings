COMPILER = ocamlc
BASE_PACKAGES = -linkpkg -package yojson -package str -package extlib
CLIENT_PACKAGES = $(BASE_PACKAGES) -package netclient -package config-file
SERVER_PACKAGES = $(BASE_PACKAGES) -package sqlite3 -package unix
BASE_DIR = ./src/base
CLIENT_DIR = ./src/client
SERVER_DIR = ./src/server
BASE_SOURCES = $(BASE_DIR)/parser.ml $(BASE_DIR)/gameinfo.ml $(BASE_DIR)/network.ml $(BASE_DIR)/teeworlds_message.ml $(BASE_DIR)/json.ml
CLIENT_SOURCES = $(CLIENT_DIR)/teeworlds_econ.ml $(CLIENT_DIR)/client.ml $(CLIENT_DIR)/main.ml
SERVER_SOURCES = $(SERVER_DIR)/db.ml $(SERVER_DIR)/rating.ml $(SERVER_DIR)/server.ml $(SERVER_DIR)/main.ml
BUILD_CLIENT = ocamlfind $(COMPILER) $(CLIENT_PACKAGES) -I $(BASE_DIR) -I $(CLIENT_DIR)
BUILD_SERVER = ocamlfind $(COMPILER) $(SERVER_PACKAGES) -I $(BASE_DIR) -I $(SERVER_DIR)
CLIENT_EXECUTABLE = send_gameinfo
SERVER_EXECUTABLE = get_gameinfo

.PHONY: db_setup

all: client_release server_release

debug: client_debug server_debug

client_release:
	$(BUILD_CLIENT) -o $(CLIENT_EXECUTABLE) $(BASE_SOURCES) $(CLIENT_SOURCES)

client_debug:
	$(BUILD_CLIENT) -g -o $(CLIENT_EXECUTABLE) $(BASE_SOURCES) $(CLIENT_SOURCES)

server_release:
	$(BUILD_SERVER) -o $(SERVER_EXECUTABLE) $(BASE_SOURCES) $(SERVER_SOURCES)

server_debug:
	$(BUILD_SERVER) -g -o $(SERVER_EXECUTABLE) $(BASE_SOURCES) $(SERVER_SOURCES)

db_setup:
	$(shell tools/db_setup.sh)

clean:
	rm -f $(CLIENT_EXECUTABLE) $(SERVER_EXECUTABLE) $(shell find . -name '*.cmo' -or -name '*.cmi' -or -name '*.cmx' -or -name '*.o')
