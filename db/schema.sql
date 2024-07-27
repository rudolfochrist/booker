CREATE TABLE db_id_generator (
  id integer PRIMARY KEY AUTOINCREMENT,
  val text UNIQUE
  
);

CREATE VIRTUAL TABLE bookmarks USING fts5(
  id UNINDEXED,
  url UNINDEXED,
  title,
  body,
  created_at UNINDEXED,
  tokenize = 'porter'
);
