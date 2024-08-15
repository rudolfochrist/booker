-- id: 20240815110630
-- direction: UP
-- description: create_bookmarks_triggers
CREATE trigger IF NOT exists bookmarks_ai after INSERT ON bookmarks
BEGIN
  INSERT INTO bookmarks_fts(rowid, title, body)
  values (new.id, new.title, new.body);
END;

--;;
CREATE trigger IF NOT exists bookmarks_ad after DELETE ON bookmarks
BEGIN
  INSERT INTO bookmarks_fts(bookmarks_fts, rowid, title, body)
  values ('delete', old.id, old.title, old.body);
END;

--;;
CREATE trigger IF NOT exists bookmarks_au after UPDATE ON bookmarks
BEGIN
  INSERT INTO bookmarks_fts(bookmarks_fts, rowid, title, body)
  values ('delete', old.id, old.title, old.body);

  INSERT INTO bookmarks_fts(rowid, title, body)
  values (new.id, new.title, new.body);
END;



