OCB = ocamlbuild
BASE_INCLUDES = -I 'src/base'
CLIENT_INCLUDES = -I 'src/client'
SERVER_INCLUDES = -I 'src/server' -I 'src/server/db'
BASE_PACKAGES = -pkg yojson -pkg extlib
CLIENT_PACKAGES = $(BASE_PACKAGES) -pkg netclient -pkg config-file
SERVER_PACKAGES = $(BASE_PACKAGES) -pkg sqlite3 -pkg unix -pkg str
BASE_FLAGS = -use-ocamlfind -I 'src/base'
CLIENT_FLAGS = $(BASE_FLAGS) $(CLIENT_INCLUDES) $(CLIENT_PACKAGES)
SERVER_FLAGS = $(BASE_FLAGS) $(SERVER_INCLUDES) $(SERVER_PACKAGES)
CLIENT_EXECUTABLE = teeworlds_ratings
SERVER_EXECUTABLE = teeworlds_ratings_srv

.PHONY: db_setup

all: client_release server_release

debug: client_debug server_debug

client_release:
	$(OCB) $(CLIENT_FLAGS) 'main.native' && mv main.native $(CLIENT_EXECUTABLE)

client_debug:
	$(OCB) $(CLIENT_FLAGS) -tag debug 'main.byte' && mv main.byte $(CLIENT_EXECUTABLE)

server_release:
	$(OCB) $(SERVER_FLAGS) 'main.native' && mv main.native $(SERVER_EXECUTABLE)

server_debug:
	$(OCB) $(SERVER_FLAGS) -tag debug 'main.byte' && mv main.byte $(SERVER_EXECUTABLE)

db_setup:
	$(shell tools/db_setup.sh)

clean:
	$(OCB) $(CLIENT_FLAGS) $(SERVER_FLAGS) -clean
