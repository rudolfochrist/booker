-- id: 20240703133547
-- direction: UP
-- description: create_table_db_id_generator


CREATE TABLE db_id_generator (
  id integer PRIMARY KEY AUTOINCREMENT,
  val text UNIQUE
  
)
