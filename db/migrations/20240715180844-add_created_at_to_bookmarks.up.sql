-- id: 20240715180844
-- direction: UP
-- description: add_created_at_to_bookmarks
CREATE TABLE bookmarks_bak (
  id integer,
  url text,
  title text,
  body text
);
--;;
INSERT INTO bookmarks_bak (id, url, title, body)
SELECT id, url, title, body
FROM bookmarks;
--;;
DROP TABLE bookmarks;
--;;
CREATE VIRTUAL TABLE bookmarks USING fts5(
  id UNINDEXED,
  url UNINDEXED,
  title,
  body,
  created_at UNINDEXED,
  tokenize = 'porter'
);
--;;
INSERT INTO bookmarks (id, url, title, body, created_at)
SELECT id, url, title, body, datetime('now')
FROM bookmarks_bak;
--;;
DROP TABLE bookmarks_bak;


