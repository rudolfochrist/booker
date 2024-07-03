-- id: 20240703114339
-- direction: UP
-- description: create_table_bookmarks


CREATE VIRTUAL TABLE bookmarks USING fts5(
  id UNINDEXED,
  url UNINDEXED,
  title,
  body,
  tokenize = 'porter'
)
