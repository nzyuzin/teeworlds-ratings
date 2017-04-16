#!/usr/bin/env bash

if [ ! -e rctf.db ]; then
  sqlite3 rctf.db < tools/create_tables.sql
else
  echo "Database already exists!"
  exit 1
fi
