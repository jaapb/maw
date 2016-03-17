CREATE TABLE game_casting (
    game_id integer NOT NULL,
    team_name character varying(50) NOT NULL,
    role_name character varying(50) NOT NULL,
    user_id integer NOT NULL
);

CREATE TABLE game_inscriptions (
    game_id integer NOT NULL,
    user_id integer NOT NULL,
    inscription_time timestamp without time zone NOT NULL,
    team_name character varying(50),
    role_type character varying(50),
    note character varying(150) NOT NULL,
    group_id integer,
    status character(1) DEFAULT 'I'::bpchar NOT NULL
);

CREATE TABLE games (
    id integer NOT NULL,
    title text NOT NULL,
    date date,
    description text DEFAULT 'No description yet'::text NOT NULL,
    location text DEFAULT 'Location TBD'::text NOT NULL,
    designer integer NOT NULL,
    min_players integer DEFAULT 0 NOT NULL,
    max_players integer DEFAULT 0 NOT NULL,
    casting_published boolean DEFAULT false NOT NULL
);

CREATE SEQUENCE games_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE games_id_seq OWNED BY games.id;

CREATE TABLE role_types (
    name character varying(50) NOT NULL,
    game_id integer NOT NULL
);

CREATE TABLE teams (
    name character varying(50) NOT NULL,
    game_id integer NOT NULL
);

CREATE TABLE users (
    id integer NOT NULL,
    name text NOT NULL,
    username character varying(32) NOT NULL,
    is_admin boolean DEFAULT false NOT NULL,
    email text NOT NULL,
		password text,
		confirmation character(32)
);

CREATE SEQUENCE users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE users_id_seq OWNED BY users.id;

ALTER TABLE ONLY games ALTER COLUMN id SET DEFAULT nextval('games_id_seq'::regclass);

ALTER TABLE ONLY users ALTER COLUMN id SET DEFAULT nextval('users_id_seq'::regclass);

ALTER TABLE ONLY game_casting
    ADD CONSTRAINT game_casting_game_id_user_id_key UNIQUE (game_id, user_id);

ALTER TABLE ONLY game_inscriptions
    ADD CONSTRAINT game_inscriptions_game_id_user_id_key UNIQUE (game_id, user_id);

ALTER TABLE ONLY teams
    ADD CONSTRAINT groups_pkey PRIMARY KEY (name, game_id);

ALTER TABLE ONLY games
    ADD CONSTRAINT megagames_pkey PRIMARY KEY (id);

ALTER TABLE ONLY role_types
    ADD CONSTRAINT role_types_pkey PRIMARY KEY (name, game_id);

ALTER TABLE ONLY users
    ADD CONSTRAINT users_name_key UNIQUE (name);

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);

ALTER TABLE ONLY game_casting
    ADD CONSTRAINT game_casting_game_id_fkey FOREIGN KEY (game_id) REFERENCES games(id);

ALTER TABLE ONLY game_casting
    ADD CONSTRAINT game_casting_game_id_fkey1 FOREIGN KEY (game_id, team_name) REFERENCES teams(game_id, name);

ALTER TABLE ONLY game_casting
    ADD CONSTRAINT game_casting_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);

ALTER TABLE ONLY game_inscriptions
    ADD CONSTRAINT game_inscriptions_game_id_fkey FOREIGN KEY (game_id) REFERENCES games(id);

ALTER TABLE ONLY game_inscriptions
    ADD CONSTRAINT game_inscriptions_group_name_fkey FOREIGN KEY (team_name, game_id) REFERENCES teams(name, game_id);

ALTER TABLE ONLY game_inscriptions
    ADD CONSTRAINT game_inscriptions_role_type_fkey FOREIGN KEY (role_type, game_id) REFERENCES role_types(name, game_id);

ALTER TABLE ONLY game_inscriptions
    ADD CONSTRAINT game_inscriptions_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);

ALTER TABLE ONLY games
    ADD CONSTRAINT games_designer_fkey FOREIGN KEY (designer) REFERENCES users(id);

ALTER TABLE ONLY teams
    ADD CONSTRAINT groups_game_id_fkey FOREIGN KEY (game_id) REFERENCES games(id);

ALTER TABLE ONLY role_types
    ADD CONSTRAINT role_types_game_id_fkey FOREIGN KEY (game_id) REFERENCES games(id);
