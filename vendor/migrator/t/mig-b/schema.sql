PRAGMA foreign_keys = 1;

CREATE TABLE Node(
  node_oid INTEGER PRIMARY KEY NOT NULL,
  node_id TEXT NOT NULL,
  something_else TEXT,
  created_at DATETIME NOT NULL DEFAULT current_timestamp
);
CREATE UNIQUE INDEX Node_node_id on Node(node_id);

CREATE TABLE Foo(
  foo_oid INTERGER PRIMARY KEY NOT NULL,
  foo_id TEXT NOT NULL,
  datum TEXT
);
CREATE UNIQUE INDEX Foo_foo_id on Foo(foo_id);

PRAGMA user_version = 1;
