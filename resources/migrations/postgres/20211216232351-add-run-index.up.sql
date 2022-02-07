ALTER TABLE runs
ADD COLUMN index JSONB;

--;;

CREATE INDEX IF NOT EXISTS runs_index ON runs USING gin (index);
