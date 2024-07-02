
DROP TABLE IF EXISTS Job;

-- Drop column 
CREATE TABLE Node_bak(
  node_oid INTEGER PRIMARY KEY NOT NULL,
  node_id TEXT NOT NULL,
  something_else TEXT);

INSERT INTO Node_bak
SELECT node_oid, node_id, something_else
FROM Node;

DROP TABLE Node;

ALTER TABLE Node_bak RENAME TO Node;


