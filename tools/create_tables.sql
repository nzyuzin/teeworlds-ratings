create table clans (
  id INTEGER PRIMARY KEY,
  name VARCHAR(12) NOT NULL UNIQUE,
  sub_clan_id INTEGER,
  FOREIGN KEY (sub_clan_id) REFERENCES clans(id)
);

create table players (
  id INTEGER PRIMARY KEY,
  name varchar(16) NOT NULL UNIQUE,
  clan_id INTEGER,
  rating int NOT NULL,
  secret_key text NOT NULL,
  FOREIGN KEY (clan_id) REFERENCES clans(id)
);

create table games (
  id INTEGER PRIMARY KEY,
  gametype varchar(8) NOT NULL,
  map varchar(16) NOT NULL,
  game_time int NOT NULL,
  game_result varchar(7) NOT NULL,
  game_date DATETIME NOT NULL
);

create table game_players (
  game_id INTEGER,
  player_id INTEGER,
  clan_id INTEGER,
  score int NOT NULL,
  team varchar(4) NOT NULL,
  rating_change int NOT NULL,
  hammer_kills int NOT NULL,
  gun_kills int NOT NULL,
  shotgun_kills int NOT NULL,
  grenade_kills int NOT NULL,
  rifle_kills int NOT NULL,
  deaths int NOT NULL,
  suicides int NOT NULL,
  flag_grabs int NOT NULL,
  flag_captures int NOT NULL,
  flag_returns int NOT NULL,
  flag_carrier_kills int NOT NULL,
  PRIMARY KEY (game_id, player_id),
  FOREIGN KEY (player_id) REFERENCES players(id),
  FOREIGN KEY (game_id) REFERENCES games(id),
  FOREIGN KEY (clan_id) REFERENCES clans(id)
);
