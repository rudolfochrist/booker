
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
$ BOOKER_ENV=production sbcl --no-userinit --load init.lisp
```

REPL:
```common-lisp
CL-USER(1): (asdf:load-system "booker" :force t) ;; user (ql:quickload "booker") if you use Quicklisp
CL-USER(2): (booker/db:migrate)
CL-USER(3): (booker:start-application t)
```
