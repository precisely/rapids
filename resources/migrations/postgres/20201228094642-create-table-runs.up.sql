CREATE TABLE IF NOT EXISTS runs (
  id UUID DEFAULT uuid_generate_v4(),
  PRIMARY KEY (id),
  -- data columns
  error BYTEA,
  next_id UUID,
  parent_run_id UUID,
  response BYTEA,
  result BYTEA,
  return_mode RETURN_MODES,
  run_response BYTEA,
  stack BYTEA,
  start_form TEXT,
  state RUN_STATES,
  suspend BYTEA,
  suspend_expires TIMESTAMP,
  -- timestamps
  created_at TIMESTAMP  NOT NULL  DEFAULT current_timestamp,
  updated_at TIMESTAMP  NOT NULL  DEFAULT current_timestamp
);

--;;

CREATE INDEX IF NOT EXISTS runs_suspend_expires ON runs (suspend_expires);
