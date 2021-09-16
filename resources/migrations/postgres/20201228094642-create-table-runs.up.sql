CREATE TABLE IF NOT EXISTS runs (
  id UUID,
  PRIMARY KEY (id),
  -- data columns
  object BYTEA,
  parent_run_id UUID,
  result TEXT,
  start_form TEXT,
  state RUN_STATES,
  suspend_expires TIMESTAMP,
  -- timestamps
  created_at TIMESTAMP  NOT NULL  DEFAULT current_timestamp,
  updated_at TIMESTAMP  NOT NULL  DEFAULT current_timestamp
);

--;;

CREATE INDEX IF NOT EXISTS runs_suspend_expires ON runs (suspend_expires);
