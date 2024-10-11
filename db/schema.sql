CREATE TABLE IF NOT EXISTS "bookmarks" (
  id integer PRIMARY key autoincrement,
  url text NOT null,
  title text NOT null,
  body text,
  created_at timestamp DEFAULT CURRENT_TIMESTAMP
);

CREATE VIRTUAL TABLE bookmarks_fts USING fts5 (
  title, body, content = 'bookmarks', content_rowid = 'id', tokenize = 'porter'
)

CREATE TRIGGER bookmarks_ai after INSERT ON bookmarks
BEGIN
  INSERT INTO bookmarks_fts(rowid, title, body)
  values (new.id, new.title, new.body);
END;

CREATE TRIGGER bookmarks_ad after DELETE ON bookmarks
BEGIN
  INSERT INTO bookmarks_fts(bookmarks_fts, rowid, title, body)
  values ('delete', old.id, old.title, old.body);
END;

CREATE TRIGGER bookmarks_au after UPDATE ON bookmarks
BEGIN
  INSERT INTO bookmarks_fts(bookmarks_fts, rowid, title, body)
  values ('delete', old.id, old.title, old.body);

  INSERT INTO bookmarks_fts(rowid, title, body)
  values (new.id, new.title, new.body);
END;
