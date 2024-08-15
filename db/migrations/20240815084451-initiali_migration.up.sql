-- id: 20240815084451
-- direction: UP
-- description: initiali_migration
CREATE TABLE IF NOT exists bookmarks_bak (
  id integer PRIMARY key autoincrement,
  url text NOT null,
  title text NOT null,
  body text,
  created_at timestamp DEFAULT CURRENT_TIMESTAMP
);

--;;
INSERT OR ignore INTO bookmarks_bak(id, url, title, body, created_at)
SELECT id, url, title, body, created_at
FROM bookmarks;




