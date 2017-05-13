#!/usr/bin/env bash

DB_NAME="teeworlds_ratings.db"

if [ ! -e ${DB_NAME} ]; then
  sqlite3 ${DB_NAME} < tools/create_tables.sql
else
  echo 'Database already exists!'
  exit 1
fi
