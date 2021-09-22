CREATE TRIGGER update_run_updated_at
BEFORE UPDATE ON runs
FOR EACH ROW
EXECUTE PROCEDURE set_updated_at_timestamp();