create table players(
  name varchar(15) PRIMARY KEY,
  clan varchar(11) NOT NULL,
  rating int NOT NULL
);

create view clans as
select clan, cast(avg(players.rating) as int) as clan_rating
from players
group by clan
order by clan_rating desc;
