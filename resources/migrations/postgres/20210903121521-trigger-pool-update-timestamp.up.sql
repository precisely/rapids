CREATE TRIGGER update_pool_updated_at
BEFORE UPDATE ON pools
FOR EACH ROW
EXECUTE PROCEDURE set_updated_at_timestamp();