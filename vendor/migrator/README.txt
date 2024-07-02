1 NAME
======

  migratory --- Simple declarative schema migration for SQLite.


2 VERSION
=========

  ,----
  | 0.1
  `----


3 SYNOPSIS
==========

  ,----
  | (sqlite:with-open-database (db "db.sqlite3")
  |   (migrator:migrate db #p"migrations/" :allow-deletions t))
  `----


4 DESCRIPTION
=============

  Migrates a database to the new schema given by the SQL text `schema`
  preserving the data.  We create any table that exists in schema,
  delete any old table that is no longer used and add/remove columns and
  indices as necessary.

  Under this scheme there are a set of changes that we can make to the
  schema and this script will handle it fine:

  1. Adding a new table
  2. Adding, deleting or modifying an index
  3. Adding a column to an existing table as long as the new column can
     be NULL or has a DEFAULT value specified.
  4. Changing a column to remove NULL or DEFAULT as long as all values
     in the database are not NULL
  5. Changing the type of a column
  6. Changing the user_version


  In addition this function is capable of:

  1. Deleting tables
  2. Deleting columns from tables

     But only if allow-deletions is non-nil.  If the new schema requires
     a column/table to be deleted and allow-deletions is T this function
     will raise an error.

     Note: When this function is called a transaction must not be held
     open on db.  A transaction will be used internally.  If you wish to
     perform additional migration steps as part of a migration use
     DBMigrator directly.

     Any internally generated rowid columns by SQLite may change values
     by this migration.

     But only if `allow-deletions' is non-nil.  If the new schema
     requires a column/table to be deleted and `allow-deletions' is nil
     this function will signal an error.

  `directory' must contain a `schema.sql' file that gets
  loaded. Optionally `directory' can contain a `pre.sql' and `post.sql'
  hooks to do some pre or post works, like data migrations etc.


5 AUTHOR
========

  Sebastian Christ (<mailto:rudolfo.christ@pm.me>)


6 LICENSE
=========

  Released under the MPL-2.0 license.


7 SEE ALSO
==========

  - [Simple declarative schema migration for SQLite]


[Simple declarative schema migration for SQLite]
<https://david.rothlis.net/declarative-schema-migration-for-sqlite/>
