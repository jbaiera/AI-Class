
create table login
(
    id  int not null primary key auto_increment,
    username varchar(30),
    password varchar(60)
);

create table games
(
    game_id int not null primary key auto_increment,
    game_type varchar(10),
    player_1 int not null,
    player_2 int not null,
    game_state varchar(160)
);

create table moves
(
    move_id int not null primary key auto_increment,
    game_id int not null,
    player int not null,
    move_number int not null,
    move varchar(10)
);

