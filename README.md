Teeworlds-ratings
=================

A system to process teeworlds game info at the end of the game.
Works with https://github.com/nzyuzin/rteeworlds rCTF game type.

Dependencies
------------

ocamlfind ocamlc sqlite3

Opam packages:
yojson extlib sqlite3 ocamlnet

Compiling
---------

Compile the program with
`make`
for the release version or with
`make debug`
for the development version.

Using
-----

`teeworlds-ratings` consists of two parts. The client and the server.

Client `send_gameinfo` will send game info of any finished rCTF game to every address specified in `destinations.txt`. Destination endpoint can also be specified with command line arguments.

To use the client, place its compiled executable `send_gameinfo` in the same directory as your rCTF server, then create `destinations.txt` file in which specify addresses in form `ip:port`. After you launch your rCTF server, all the results of ended games will be sent to these destinations.

The server will wait for game information and write it to the database file. This database file is not provided in the repository, but can easily be obtained by running `./tools/db_setup.sh`. The server executable should be placed along the database.

Both client and server can be used independently of each other, as long as properly formatted messages are transfered. Messages that are communicated between client and server are transfered in json format, structure of which can be found at the head of `src/base/json.mli` file. Format of the input data for the client is given in `src/client/main.ml`.
