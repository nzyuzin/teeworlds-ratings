create table players (
  id INTEGER PRIMARY KEY,
  name varchar(16) NOT NULL UNIQUE,
  clan varchar(12) NOT NULL,
  rating int NOT NULL
);

create view clans as
select clan, cast(avg(players.rating) as int) as clan_rating
from players
group by clan
order by clan_rating desc;

create table games (
  id INTEGER PRIMARY KEY,
  gametype varchar(8) NOT NULL,
  map varchar(16) NOT NULL,
  game_time int NOT NULL,
  game_result varchar(7) NOT NULL,
  game_date text NOT NULL
);

create table game_players (
  game_id INTEGER,
  player_id INTEGER,
  score int NOT NULL,
  team varchar(4) NOT NULL,
  rating_change int NOT NULL,
  PRIMARY KEY (game_id, player_id),
  FOREIGN KEY (player_id) REFERENCES player(id),
  FOREIGN KEY (game_id) REFERENCES game(id)
);
