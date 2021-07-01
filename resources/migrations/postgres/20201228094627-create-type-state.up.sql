DO $$ BEGIN
  CREATE TYPE RUN_STATES AS ENUM ('created', 'suspended', 'complete', 'error');
EXCEPTION
  WHEN duplicate_object THEN null;
END $$;
