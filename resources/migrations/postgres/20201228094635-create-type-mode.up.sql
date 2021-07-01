DO $$ BEGIN
  CREATE TYPE RETURN_MODES AS ENUM ('block', 'redirect');
EXCEPTION
  WHEN duplicate_object THEN null;
END $$;
