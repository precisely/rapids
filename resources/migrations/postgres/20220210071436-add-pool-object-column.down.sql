ALTER TABLE pools
DROP COLUMN object,
ADD COLUMN sources BYTEA,
ADD COLUMN buffer BYTEA,
ADD COLUMN sinks BYTEA;