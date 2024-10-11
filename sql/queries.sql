-- name: all-bookmarks
SELECT id, url, title
FROM bookmarks
ORDER by created_at DESC
;

-- name: find-bookmark?
SELECT id, title, url
FROM bookmarks
WHERE id = :id
;

-- name: find-bookmark-with-body?
SELECT id, title, url, body
FROM bookmarks
WHERE id = :id
;

-- name: search-bookmarks
SELECT id, url, title
FROM bookmarks
WHERE id IN (SELECT rowid
             FROM bookmarks_fts
             WHERE bookmarks_fts MATCH :q)
;

-- name: create-bookmark<!
INSERT INTO bookmarks(id, title, url, body)
VALUES
(:id, :title, :url, :body)
;

-- name: update-bookmark!
UPDATE bookmarks
SET title = :title
    url = :url
WHERE id = :id
;

-- name: delete-bookmark!
DELETE FROM bookmarks
WHERE id = :id
;
