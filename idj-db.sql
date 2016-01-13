CREATE TABLE IF NOT EXISTS custom_user (
    id INTEGER PRIMARY KEY,
    user_id INTEGER REFERENCES snap_auth_user (uid),
    real_name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS game (
    id INTEGER PRIMARY KEY,
    year INTEGER NOT NULL,
    title TEXT NOT NULL,
    description TEXT NOT NULL,
    approved BOOLEAN NOT NULL
);

CREATE TABLE IF NOT EXISTS  award_category (
    id INTEGER PRIMARY KEY,
    description TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS game_awards (
    id INTEGER PRIMARY KEY,
    place INTEGER NOT NULL,
    category_id INTEGER REFERENCES award_category (id),
    game_id INTEGER REFERENCES game (id)
);

CREATE TABLE IF NOT EXISTS posts (
    id INTEGER NOT NULL,
    content TEXT NOT NULL,
    author INTEGER REFERENCES snap_auth_user (uid)
);

INSERT INTO award_category (description)
VALUES ('Jogo');
INSERT INTO award_category (description)
VALUES ('Arte / Design');
INSERT INTO award_category (description)
VALUES ('Trilha sonora');
INSERT INTO award_category (description)
VALUES ('Programação');
