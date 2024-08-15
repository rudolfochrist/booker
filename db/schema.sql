CREATE TABLE sqlite_sequence(name,seq);
CREATE TABLE migration (
    id BIGINT PRIMARY KEY,
    kind CHARACTER VARYING(255) NOT NULL,
    description CHARACTER VARYING(4096) NOT NULL,
    applied TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
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
/* bookmarks_fts(title,body) */;
CREATE TABLE IF NOT EXISTS 'bookmarks_fts_data'(id INTEGER PRIMARY KEY, block BLOB);
CREATE TABLE IF NOT EXISTS 'bookmarks_fts_idx'(segid, term, pgno, PRIMARY KEY(segid, term)) WITHOUT ROWID;
CREATE TABLE IF NOT EXISTS 'bookmarks_fts_docsize'(id INTEGER PRIMARY KEY, sz BLOB);
CREATE TABLE IF NOT EXISTS 'bookmarks_fts_config'(k PRIMARY KEY, v) WITHOUT ROWID;
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

-- cl-migration schema migrations
INSERT INTO migration (id, kind, description, applied) VALUES (20240815110630, 'SQL', 'create_bookmarks_triggers', '2024-08-15 11:26:04');
