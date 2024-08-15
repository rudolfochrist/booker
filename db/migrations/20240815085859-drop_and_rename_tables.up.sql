-- id: 20240815085859
-- direction: UP
-- description: drop_and_rename_tables
DROP TABLE IF exists bookmarks;

--;;
DROP TABLE IF exists db_id_generator;

--;;
ALTER TABLE bookmarks_bak rename TO bookmarks;

