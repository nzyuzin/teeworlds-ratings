Teeworlds-ratings
=================

A system to process teeworlds game info at the end of the game.
Works with https://github.com/nzyuzin/teeworlds rCTF game type.

Dependencies
------------

ocamlfind, ocamlc, sqlite3

Packages:
Yojson, Extlib, Sqlite3

Compiling
---------

Compile the program with
`make`
for the release version or with
`make debug`
for the development version.

Using
-----

teeworlds-ratings will send game info of any finished rCTF game to every address specified in `destinations.txt`. Destination endpoint can also be specified with command line arguments.

Messages will be transfered in json format, structure of which can be found at the head of json.mli file.

To use the program, place its compiled executable in the same directory as your rCTF server, then create `destinations.txt` file in which specify addresses in form `ip:port`. After you launch your rCTF server, all the results of ended games will be sent to these destinations.
