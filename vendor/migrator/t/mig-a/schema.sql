PRAGMA foreign_keys = 1;

CREATE TABLE Node(
  node_oid INTEGER PRIMARY KEY NOT NULL,
  node_id TEXT NOT NULL,
  active BOOLEAN NOT NULL DEFAULT(1),
  something_else TEXT);
CREATE UNIQUE INDEX Node_node_id on Node(node_id);

INSERT INTO Node (node_id, something_else)
VALUES
('123', 'old');

CREATE TABLE Job(
  node_oid INTEGER NOT NULL,
  id INTEGER NOT NULL,
  FOREIGN KEY(node_oid) REFERENCES Node(node_oid));
CREATE UNIQUE INDEX Job_node_oid on Job(node_oid, id);
