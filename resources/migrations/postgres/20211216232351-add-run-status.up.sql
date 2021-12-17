ALTER TABLE runs
ADD COLUMN status JSONB;

--;;

CREATE INDEX IF NOT EXISTS runs_status ON runs USING gin (status);
