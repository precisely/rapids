DO $$ BEGIN
  CREATE TYPE RUN_STATES AS ENUM ('running', 'complete', 'error');
EXCEPTION
  WHEN duplicate_object THEN null;
END $$;
