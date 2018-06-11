-- README:
-- Do not remove the field with a `-- DEFAULT` suffix.
-- That's the default tables/fields needed by Ocsigen-start

CREATE DATABASE ocsipersist_maw;

CREATE EXTENSION citext; --DEFAULT
-- You may remove the above line if you use the type TEXT for emails instead of CITEXT

CREATE SCHEMA ocsigen_start
  CREATE TABLE users ( -- DEFAULT
         userid bigserial primary key, -- DEFAULT
         firstname text NOT NULL,
         lastname text NOT NULL,
         main_email citext,
         password text,
         avatar text,
         language text,
         admin boolean NOT NULL DEFAULT(false)
  )

  CREATE TABLE emails ( -- DEFAULT
         email citext primary key, -- DEFAULT
         userid bigint NOT NULL references users(userid), -- DEFAULT
         validated boolean NOT NULL DEFAULT(false)
  )

  CREATE TABLE activation ( -- DEFAULT
         activationkey text primary key, -- DEFAULT
         userid bigint NOT NULL references users(userid), -- DEFAULT
         email citext NOT NULL,
         autoconnect boolean NOT NULL,
         validity bigint NOT NULL,
         action text NOT NULL,
         data text NOT NULL,
         creationdate timestamptz NOT NULL default now()
  )

  CREATE TABLE groups ( -- DEFAULT
         groupid bigserial primary key, -- DEFAULT
         name text NOT NULL, -- DEFAULT
         description text -- DEFAULT
  )

  CREATE TABLE user_groups ( -- DEFAULT
         userid bigint NOT NULL references users(userid), -- DEFAULT
         groupid bigint NOT NULL references groups(groupid) -- DEFAULT
  )

  CREATE TABLE preregister (
         email citext NOT NULL
  )

  CREATE TABLE phones (
       number citext primary key,
       userid bigint NOT NULL references users(userid)
  );

CREATE SCHEMA maw
	CREATE TABLE games (
         id bigserial primary key,
         title text NOT NULL,
         location text,
         date date,
         blurb text
  );

	CREATE TABLE game_designers (
         game_id bigint references games(id),
         userid bigint references users(userid)
         primary key (game_id, userid)
  );
