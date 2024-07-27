
# booker

A simple bookmarking application with full-text search.

## Installation

Firstly, add a configuration file to `config/ENV.lisp` with `ENV`
being the environment the application is running
in. E.g. `development`, `production`, etc.

See `lib/config.lisp` for configuration options.

```sh
$ git clone --recurse-submodules https://github.com/rudolfochrist/booker.git
$ install-dependecies # skip this if you're using Quicklisp. See: https://github.com/rudolfochrist/project-loader
```

DB migrations are handled manually. If you start a fresh DB

```sh
$ sqlite3 db/booker.db < db/schema.sql
```

is enough. If you have already a running instance with data, then you
have to copy it over manually. If dependencies are installed and
migrations applied then run

```sh
$ BOOKER_ENV=production sbcl --no-userinit --load init.lisp
```

to start the REPL. Then

REPL:
```common-lisp
CL-USER: (asdf:load-system "booker" :force t) ;; user (ql:quickload "booker") if you use Quicklisp
CL-USER: (booker:start-application t)
```
