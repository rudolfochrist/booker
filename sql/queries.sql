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
SELECT id, url, title,
       ts_rank(search, websearch_to_tsquery('english', :q)) +
       ts_rank(search, websearch_to_tsquery('simple', :q)) AS rank
FROM bookmarks
WHERE search @@ websearch_to_tsquery('english', :q)
OR search @@ websearch_to_tsquery('simple', :q)
ORDER by rank DESC
;

-- name: create-bookmark<!
INSERT INTO bookmarks(title, url, body)
VALUES
(:title, :url, :body)
RETURNING id
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
