CREATE TABLE IF NOT EXISTS pools (
  id UUID, -- code will create UUID DEFAULT uuid_generate_v4(),
  PRIMARY KEY (id),
  -- data columns
  size smallint,
  sources BYTEA,
  buffer BYTEA,
  sinks BYTEA
  -- timestamps
  created_at TIMESTAMP  NOT NULL  DEFAULT current_timestamp,
  updated_at TIMESTAMP  NOT NULL  DEFAULT current_timestamp
);

