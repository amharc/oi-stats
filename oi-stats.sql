DROP INDEX IF EXISTS submission_participation;
CREATE INDEX submission_participation ON submission (participation);

DROP INDEX IF EXISTS participation_stage_sum;
CREATE INDEX participation_stage_sum ON participation (stage, sum DESC NULLS LAST);

DROP INDEX IF EXISTS participation_contestant_sum;
CREATE INDEX participation_contestant_sum ON participation (contestant, sum DESC NULLS LAST);

DROP INDEX IF EXISTS participation_stage_contestant;
CREATE INDEX participation_stage_contestant ON participation (stage, contestant);

DROP INDEX IF EXISTS participation_sum_stage;
CREATE INDEX participation_sum_stage ON participation (sum, stage, contestant);

DROP INDEX IF EXISTS participation_school_contestant;
CREATE INDEX participation_school_contestant ON participation (school, contestant, stage);

DROP INDEX IF EXISTS contestant_city;
CREATE INDEX contestant_city ON contestant (city, surname, name);

DROP INDEX IF EXISTS contestant_name;
CREATE INDEX contestant_name ON contestant (name, surname, city);

DROP INDEX IF EXISTS task_stage;
CREATE INDEX task_stage ON task (stage, sample, name);

ALTER TABLE submission DROP CONSTRAINT IF EXISTS __manual_submission_check_result;
ALTER TABLE submission ADD CONSTRAINT __manual_submission_check_result CHECK (result >= 0 AND result <= 100);

ALTER TABLE participation DROP CONSTRAINT IF EXISTS __manual_participation_check_award;
ALTER TABLE participation ADD CONSTRAINT __manual_participation_check_award CHECK (award IS NULL OR (award >= 0 AND award <= 4));

CLUSTER VERBOSE submission USING unique_submission;
CLUSTER VERBOSE participation USING participation_stage_sum;

VACUUM (FULL, ANALYZE) city;
VACUUM (FULL, ANALYZE) submission;
VACUUM (FULL, ANALYZE) participation;
VACUUM (FULL, ANALYZE) contestant;
VACUUM (FULL, ANALYZE) olympiad;
VACUUM (FULL, ANALYZE) stage;

CREATE OR REPLACE FUNCTION check_participation_city_fun() RETURNS trigger AS '
    DECLARE
        school_city bigint;
        contestant_city bigint;
        contestant_name character varying;
        contestant_surname character varying;
    BEGIN
        IF NEW.school IS NULL THEN
            RETURN NEW;
        ELSE
            SELECT city, name, surname INTO contestant_city, contestant_name, contestant_surname FROM contestant WHERE contestant.id = NEW.contestant;
            SELECT city INTO school_city FROM school WHERE school.id = NEW.school;
            IF school_city = contestant_city THEN
                RETURN NEW;
            ELSE
                RAISE EXCEPTION ''City mismatch in participation % of contestant %: % %, namely school: % vs contestant: %d'', NEW.id, NEW.contestant, contestant_name, contestant_surname, school_city, contestant_city;
            END IF;
        END IF;
    END;'
    LANGUAGE 'plpgsql';

DROP TRIGGER IF EXISTS check_participation_city ON participation;
CREATE CONSTRAINT TRIGGER check_participation_city AFTER INSERT OR UPDATE ON participation DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE check_participation_city_fun();

CREATE OR REPLACE FUNCTION check_participation_award_finals_fun() RETURNS trigger AS '
    DECLARE
        num bigint;
    BEGIN
        SELECT stage INTO num FROM stage WHERE id = NEW.stage;
        IF num = 3 THEN
            RAISE EXCEPTION ''Award was awarded in non-finals'';
        ELSE
            RETURN NEW;
        END IF;
    END;'
    LANGUAGE 'plpgsql';

DROP TRIGGER IF EXISTS check_participation_award_finals ON participation;
CREATE CONSTRAINT TRIGGER check_participation_award_finals AFTER INSERT OR UPDATE ON participation FOR EACH ROW WHEN (NEW.award IS NULL) EXECUTE PROCEDURE check_participation_award_finals_fun();

CREATE OR REPLACE FUNCTION check_participation_award_nonfinals_fun() RETURNS trigger AS '
    DECLARE
        num bigint;
    BEGIN
        SELECT stage INTO num FROM stage WHERE id = NEW.stage;
        IF num = 3 THEN
            RETURN NEW;
        ELSE
            RAISE EXCEPTION ''Award was not awarded in finals'';
        END IF;
    END;'
    LANGUAGE 'plpgsql';

DROP TRIGGER IF EXISTS check_participation_award_nonfinals ON participation;
CREATE CONSTRAINT TRIGGER check_participation_award_nonfinals AFTER INSERT OR UPDATE ON participation FOR EACH ROW WHEN (NEW.award IS NOT NULL) EXECUTE PROCEDURE   check_participation_award_nonfinals_fun();

CREATE OR REPLACE FUNCTION prune_after_deletion_fun() RETURNS trigger AS '
    BEGIN
        DELETE FROM school WHERE NOT EXISTS (SELECT id FROM participation WHERE school.id = participation.school);
        DELETE FROM contestant WHERE NOT EXISTS (SELECT id FROM participation WHERE contestant.id = participation.contestant);
        DELETE FROM city WHERE NOT EXISTS (SELECT id FROM contestant WHERE city.id = contestant.city);
        RETURN NEW;
    END;'
    LANGUAGE 'plpgsql';

DROP TRIGGER IF EXISTS prune_after_stage_deletion ON stage;
CREATE TRIGGER prune_after_stage_deletion AFTER DELETE OR TRUNCATE ON stage EXECUTE PROCEDURE prune_after_deletion_fun();

CREATE OR REPLACE FUNCTION check_submission_stage_fun() RETURNS trigger AS '
    DECLARE
        task_stage bigint;
        participation_stage bigint;
    BEGIN
        SELECT stage INTO task_stage FROM task WHERE task.id = NEW.task;
        SELECT stage INTO participation_stage FROM participation WHERE participation.id = NEW.participation;
        IF task_stage = participation_stage THEN
            RETURN NEW;
        ELSE
            RAISE EXCEPTION ''Stage mismatch in submission %, namely task: % vs participation: %'', NEW.id, task_stage, participation_stage;
        END IF;
    END;'
    LANGUAGE 'plpgsql';

DROP TRIGGER IF EXISTS check_submission_stage ON submission;
CREATE CONSTRAINT TRIGGER check_submission_stage AFTER INSERT OR UPDATE ON submission FOR EACH ROW EXECUTE PROCEDURE check_submission_stage_fun();

CREATE OR REPLACE FUNCTION check_participation_sum_fun() RETURNS trigger AS '
    DECLARE
        real_sum bigint;
        contestant_name character varying;
        contestant_surname character varying;
    BEGIN
        IF NEW.sum IS NULL THEN
            RETURN NEW;
        ELSE
            SELECT SUM(result) INTO real_sum FROM submission INNER JOIN task ON submission.task = task.id WHERE participation = NEW.id AND NOT task.sample;
            IF real_sum = NEW.sum THEN
                RETURN NEW;
            ELSE
                SELECT name, surname INTO contestant_name, contestant_surname FROM contestant WHERE NEW.contestant = contestant.id;
                RAISE EXCEPTION ''Sum mismatch in participation % of contestant % %, provided: %, calculated: %'', NEW.id, contestant_name, contestant_surname, NEW.sum, real_sum;
            END IF;
        END IF;
    END;'
    LANGUAGE 'plpgsql';

DROP TRIGGER IF EXISTS check_participation_sum ON participation;
CREATE CONSTRAINT TRIGGER check_participation_sum AFTER INSERT OR UPDATE ON participation DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE PROCEDURE check_participation_sum_fun();

CREATE OR REPLACE FUNCTION generate_ranking() RETURNS VOID AS '
    BEGIN
        DROP TABLE IF EXISTS ranking;
        CREATE TABLE ranking AS
            SELECT  participation.stage AS stage,
                    submission.id AS id,
                    rank() OVER (PARTITION BY participation.stage, task.id ORDER BY participation.sum DESC NULLS LAST) AS position,
                    participation.award AS award,
                    contestant.name AS name,
                    contestant.surname AS surname,
                    contestant.id AS contestantid,
                    participation.class AS class,
                    participation.school AS schoolid,
                    school.name AS school,
                    city.name AS city,
                    city.latitude AS latitude,
                    city.longitude AS longitude,
                    participation.sum AS sum,
                    task.id AS task,
                    task.sample AS tasksample,
                    task.name AS taskname,
                    submission.result AS result
                FROM participation
                     INNER JOIN contestant ON participation.contestant = contestant.id
                     INNER JOIN city ON contestant.city = city.id
                     INNER JOIN submission ON participation.id = submission.participation
                     INNER JOIN task ON submission.task = task.id
                     LEFT OUTER JOIN school ON participation.school = school.id
                ORDER BY
                    participation.stage ASC,
                    COALESCE(participation.sum, -1) DESC,
                    contestant.surname ASC,
                    contestant.name ASC,
                    contestant.id ASC,
                    task.sample DESC,
                    task.name ASC,
                    task.id ASC;
        CREATE INDEX ranking_stage ON ranking(stage, sum DESC NULLS LAST, surname, name, contestantid, tasksample DESC, taskname, task);
    END;'
    LANGUAGE 'plpgsql';

VACUUM (FULL, ANALYZE) ranking;
