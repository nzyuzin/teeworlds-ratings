Teeworlds-ratings
=================

A system to process teeworlds game info at the end of the game.
Works with https://github.com/nzyuzin/rteeworlds rCTF game type.

Dependencies
------------

ocamlfind ocamlc sqlite3

Opam packages:
yojson extlib sqlite3 ocamlnet config-file

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

Client `teeworlds_ratings` will send game info of any finished rCTF game to the address specified in the configuration file.

To use the client, place its compiled executable `teeworlds_ratings` in the same directory as your rCTF server, then create `teeworlds_ratings.conf` file or use the one provided in examples. There you need to specify econ port and password of the teeworlds server.

The server `teeworlds_ratings_srv` will wait for game information and write it to the database file. This database file is not provided in the repository, but can easily be obtained by running `make db_setup`. The server executable should be placed along the database.

Both client and server can be used independently of each other, as long as properly formatted messages are transfered. Messages that are communicated between client and server are transfered in json format, structure of which can be found at the head of `src/base/json.mli` file. Format of the input data for the client is given in `src/base/teeworlds_messages.ml`.
