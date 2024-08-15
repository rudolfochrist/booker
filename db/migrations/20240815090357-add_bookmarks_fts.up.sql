-- id: 20240815090357
-- direction: UP
-- description: add_bookmarks_fts
CREATE virtual TABLE IF NOT exists bookmarks_fts USING fts5 (
  title, body, content = 'bookmarks', content_rowid = 'id', tokenize = 'porter'
);

--;;
-- Rebuild search index from content table
INSERT INTO bookmarks_fts(bookmarks_fts) values ('rebuild');

