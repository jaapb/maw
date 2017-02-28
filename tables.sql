CREATE TABLE game_casting (
    game_id integer NOT NULL,
    user_id integer,
    team_name text NOT NULL,
    role_name text NOT NULL
);

CREATE TABLE game_inscriptions (
    game_id integer NOT NULL,
    user_id integer NOT NULL,
    inscription_time timestamp without time zone DEFAULT now() NOT NULL,
    note text NOT NULL,
    group_name text,
    status integer DEFAULT 3 NOT NULL,
    preferred_team text,
    preferred_role text,
    cancelled boolean DEFAULT false NOT NULL
);

CREATE TABLE games (
    id integer NOT NULL,
    title text NOT NULL,
    date date,
    description text DEFAULT 'No description yet.'::text NOT NULL,
    location text DEFAULT 'Location TBD'::text NOT NULL,
    designer integer NOT NULL,
    min_players integer DEFAULT 0 NOT NULL,
    max_players integer DEFAULT 0 NOT NULL,
    casting_published boolean DEFAULT false NOT NULL,
    inscription_deadline date,
    cancellation_deadline date,
    payment_deadline date,
    bookable boolean DEFAULT false NOT NULL,
    visible boolean DEFAULT false NOT NULL,
    gate_list_closed boolean DEFAULT false NOT NULL,
    CONSTRAINT games_check CHECK ((inscription_deadline <= date)),
    CONSTRAINT games_check1 CHECK ((cancellation_deadline <= date)),
    CONSTRAINT games_check2 CHECK ((payment_deadline <= date))
);

CREATE SEQUENCE games_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE games_id_seq OWNED BY games.id;

CREATE TABLE provisional_users (
    id integer NOT NULL,
    email text NOT NULL,
    game_id integer,
    first_name text NOT NULL,
    last_name text NOT NULL
);

CREATE TABLE users (
    id integer NOT NULL,
    last_name text NOT NULL,
    is_admin boolean DEFAULT false NOT NULL,
    email text NOT NULL,
    password text NOT NULL,
    confirmation character(32),
    password_salt character varying(8) NOT NULL,
    first_name text NOT NULL,
    address text NOT NULL,
    phone_number text NOT NULL,
    town text NOT NULL,
    postcode text NOT NULL,
    country text NOT NULL,
    hidden boolean DEFAULT false NOT NULL
);

CREATE SEQUENCE users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

COPY users (id, name, username, is_admin, email, password, confirmation, password_salt) FROM stdin;
1      Administrator   admin   t       root@kerguelen.org      tdfBqVvihMVZzMwYXZ03CAatorP7Ef7Uj7Id3C4OUV3pQ0Lnts5F8+OeNvNktq3UBakUCrVh2HPNc2KQQNihvA  \N      xxxxxxxx
\.

ALTER SEQUENCE users_id_seq OWNED BY users.id;

CREATE TABLE user_ids (
    id integer DEFAULT nextval('users_id_seq'::regclass) NOT NULL,
    creation_time timestamp without time zone DEFAULT now() NOT NULL
);

ALTER TABLE ONLY games ALTER COLUMN id SET DEFAULT nextval('games_id_seq'::regclass);

ALTER TABLE ONLY game_casting
    ADD CONSTRAINT game_casting_game_id_team_name_role_name_key UNIQUE (game_id, team_name, role_name);

ALTER TABLE ONLY game_casting
    ADD CONSTRAINT game_casting_game_id_user_id_key UNIQUE (game_id, user_id);

ALTER TABLE ONLY game_inscriptions
    ADD CONSTRAINT game_inscriptions_game_id_user_id_key UNIQUE (game_id, user_id);

ALTER TABLE ONLY game_inscriptions
    ADD CONSTRAINT game_inscriptions_user_id_game_id_key UNIQUE (user_id, game_id);

ALTER TABLE ONLY games
    ADD CONSTRAINT megagames_pkey PRIMARY KEY (id);

ALTER TABLE ONLY provisional_users
    ADD CONSTRAINT provisional_users_pkey PRIMARY KEY (id);

ALTER TABLE ONLY user_ids
    ADD CONSTRAINT user_ids_pkey PRIMARY KEY (id);

ALTER TABLE ONLY users
    ADD CONSTRAINT users_email_key UNIQUE (email);

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);

ALTER TABLE ONLY game_casting
    ADD CONSTRAINT game_casting_game_id_fkey FOREIGN KEY (game_id) REFERENCES games(id);

ALTER TABLE ONLY game_casting
    ADD CONSTRAINT game_casting_user_id_fkey FOREIGN KEY (user_id) REFERENCES user_ids(id);

ALTER TABLE ONLY game_inscriptions
    ADD CONSTRAINT game_inscriptions_game_id_fkey FOREIGN KEY (game_id) REFERENCES games(id);

ALTER TABLE ONLY game_inscriptions
    ADD CONSTRAINT game_inscriptions_user_id_fkey FOREIGN KEY (user_id) REFERENCES user_ids(id);

ALTER TABLE ONLY games
    ADD CONSTRAINT games_designer_fkey FOREIGN KEY (designer) REFERENCES user_ids(id);

ALTER TABLE ONLY provisional_users
    ADD CONSTRAINT provisional_users_game_id_fkey FOREIGN KEY (game_id) REFERENCES games(id);

ALTER TABLE ONLY provisional_users
    ADD CONSTRAINT provisional_users_id_fkey FOREIGN KEY (id) REFERENCES user_ids(id);

ALTER TABLE ONLY users
    ADD CONSTRAINT users_id_fkey FOREIGN KEY (id) REFERENCES user_ids(id);
