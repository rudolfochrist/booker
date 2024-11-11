-- migrate:up
CREATE TABLE bookmarks (
  id serial PRIMARY key,
  url text UNIQUE NOT null,
  title text NOT null,
  body text,
  created_at timestamptz DEFAULT now(),
  search tsvector generated always AS (
    setweight(to_tsvector('english',title), 'A')  || ' ' ||
    setweight(to_tsvector('english',body), 'B') || ' ' ||
    setweight(to_tsvector('simple',url), 'C') :: tsvector
  ) stored
);


CREATE index idx_search ON bookmarks USING GIN(search);

-- migrate:down

