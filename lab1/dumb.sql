CREATE TABLE person (
    id SERIAL PRIMARY KEY,
    first_name VARCHAR(127) NOT NULL,
    last_name VARCHAR(127) NOT NULL,
    position VARCHAR(127) NOT NULL
);

CREATE TABLE section (
    id SERIAL PRIMARY KEY,
    title VARCHAR(127) NOT NULL
);

CREATE TABLE competition (
    id SERIAL PRIMARY KEY,
    section_id SERIAL REFERENCES section(id),
    title VARCHAR(127) NOT NULL
);


CREATE TABLE section_participant (
    id SERIAL PRIMARY KEY,
    section_id SERIAL REFERENCES section(id),
    person_id SERIAL REFERENCES person(id)
);

CREATE TABLE section_schedule (
    id SERIAL PRIMARY KEY,
    section_id SERIAL REFERENCES section(id),
    day_of_week INTEGER NOT NULL,
    time_start TIME NOT NULL,
    time_end TIME NOT NULL
);

CREATE TABLE competition_plan (
    id SERIAL PRIMARY KEY,
    competition_id SERIAL REFERENCES competition(id),
    competition_date TIMESTAMP NOT NULL,
    competition_winner_id INTEGER REFERENCES person
);

CREATE TABLE competition_participant (
    id SERIAL PRIMARY KEY,
    person_id SERIAL REFERENCES person(id),
    competition_plan_id SERIAL REFERENCES competition_plan(id),
    competition_result INTEGER
);




INSERT INTO person (first_name, last_name, position) VALUES ('Oleg', 'Lipskiy', 'Student');
INSERT INTO person (first_name, last_name, position) VALUES ('Sport', 'Sman', 'Student');
INSERT INTO person (first_name, last_name, position) VALUES ('Newst', 'Udent', 'Student');
INSERT INTO person (first_name, last_name, position) VALUES ('Tea', 'Cher', 'Teacher');

INSERT INTO section (title) VALUES ('Football');
INSERT INTO section (title) VALUES ('BasketBall');
INSERT INTO section (title) VALUES ('Hockey');
INSERT INTO section (title) VALUES ('Ping Pong');
INSERT INTO section (title) VALUES ('Chess');

INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (1, 0, '09:00', '12:00');
INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (1, 2, '10:00', '13:00');
INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (1, 4, '09:00', '12:00');

INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (2, 0, '08:30', '10:00');
INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (2, 1, '08:30', '10:00');
INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (2, 4, '08:30', '10:00');

INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (3, 1, '16:00', '19:00');
INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (3, 2, '16:00', '19:00');
INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (3, 3, '16:00', '19:00');
INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (3, 4, '16:00', '19:00');

INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (4, 0, '11:00', '13:00');
INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (4, 1, '11:00', '13:00');
INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (4, 2, '11:00', '13:00');
INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (4, 3, '10:00', '12:00');
INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (4, 4, '11:00', '13:00');
INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (4, 5, '11:00', '13:00');


INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (5, 1, '08:00', '11:00');
INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (5, 2, '08:00', '11:00');
INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (5, 3, '08:00', '11:00');
INSERT INTO section_schedule (section_id, day_of_week, time_start, time_end) VALUES (5, 4, '08:00', '11:00');

INSERT INTO section_participant (person_id, section_id) VALUES (1, 1);
INSERT INTO section_participant (person_id, section_id) VALUES (1, 4);

INSERT INTO section_participant (person_id, section_id) VALUES (2, 2);
INSERT INTO section_participant (person_id, section_id) VALUES (2, 3);
INSERT INTO section_participant (person_id, section_id) VALUES (2, 4);

INSERT INTO section_participant (person_id, section_id) VALUES (3, 1);
INSERT INTO section_participant (person_id, section_id) VALUES (3, 5);

INSERT INTO section_participant (person_id, section_id) VALUES (4, 2);
INSERT INTO section_participant (person_id, section_id) VALUES (4, 5);


INSERT INTO competition (section_id, title) VALUES (1, 'Weekly Football Challenge');
INSERT INTO competition (section_id, title) VALUES (1, 'Monthly Football Challenge');
INSERT INTO competition (section_id, title) VALUES (2, 'Baskeyball Derby');
INSERT INTO competition (section_id, title) VALUES (3, 'Hockey Univercity Championship');
INSERT INTO competition (section_id, title) VALUES (4, 'City Ping-pong League');
INSERT INTO competition (section_id, title) VALUES (5, 'Chess Tournament');

INSERT INTO competition_plan (competition_id, competition_date, competition_winner_id) VALUES (1, '2017-12-03 12:00:00', NULL);
INSERT INTO competition_plan (competition_id, competition_date, competition_winner_id) VALUES (1, '2017-12-10 12:00:00', NULL);
INSERT INTO competition_plan (competition_id, competition_date, competition_winner_id) VALUES (1, '2017-12-17 12:00:00', NULL);
INSERT INTO competition_plan (competition_id, competition_date, competition_winner_id) VALUES (1, '2017-12-24 12:00:00', NULL);

INSERT INTO competition_plan (competition_id, competition_date, competition_winner_id) VALUES (2, '2017-12-04 12:00:00', NULL);
INSERT INTO competition_plan (competition_id, competition_date, competition_winner_id) VALUES (2, '2018-01-04 12:00:00', NULL);
INSERT INTO competition_plan (competition_id, competition_date, competition_winner_id) VALUES (2, '2018-02-04 12:00:00', NULL);

INSERT INTO competition_plan (competition_id, competition_date, competition_winner_id) VALUES (3, '2018-03-14 18:00:00', NULL);
INSERT INTO competition_plan (competition_id, competition_date, competition_winner_id) VALUES (3, '2018-07-14 18:00:00', NULL);

INSERT INTO competition_plan (competition_id, competition_date, competition_winner_id) VALUES (4, '2018-05-21 16:30:00', NULL);

INSERT INTO competition_plan (competition_id, competition_date, competition_winner_id) VALUES (5, '2018-08-15 10:30:00', NULL);

INSERT INTO competition_plan (competition_id, competition_date, competition_winner_id) VALUES (6, '2017-12-05 08:30:00', NULL);
INSERT INTO competition_plan (competition_id, competition_date, competition_winner_id) VALUES (6, '2018-01-06 08:30:00', NULL);
INSERT INTO competition_plan (competition_id, competition_date, competition_winner_id) VALUES (6, '2018-02-04 08:30:00', NULL);
INSERT INTO competition_plan (competition_id, competition_date, competition_winner_id) VALUES (6, '2018-03-06 08:30:00', NULL);


INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (1, 1);
INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (1, 5);
INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (1, 6);
INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (1, 11);

INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (2, 8);
INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (2, 11);

INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (3, 1);
INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (3, 2);
INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (3, 5);
INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (3, 6);
INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (3, 12);
INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (3, 13);
INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (3, 14);

INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (4, 8);
INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (4, 12);
INSERT INTO competition_participant (person_id, competition_plan_id) VALUES (4, 13);


