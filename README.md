# cl-stack-http

requests / httpx-shaped HTTP client for Common Lisp — thin layer over
[`http-protocol`](https://github.com/egao1980/http-protocol) with platform-selectable
backends, JSON + S-expression serdes, and
[`cl-stack-pathlib`](https://github.com/egao1980/cl-stack-pathlib) upload/download.

Package: `cl-stack-http` (nick `stack-http`).

**Auth layering** (Python-shaped):

| Concern | Package |
|---------|---------|
| Wire `:basic` / `:bearer`, Digest, netrc, CLOS auth protocol | this package |
| OAuth2 get/refresh/scopes/PKCE/401 retry | [`cl-stack-oauth2`](https://github.com/egao1980/cl-stack-oauth2) |
| JWT encode/decode/verify | [`cl-stack-jwt`](https://github.com/egao1980/cl-stack-jwt) (jose) |

CLOS protocol: `auth-object-p` / `prepare-auth` / `handle-auth-response` (see `src/auth-protocol.lisp`).

## Install

```lisp
;; via cl-repository (OCI)
(cl-repo:load-system "cl-stack-http" :version "0.1.1")
```

OCI: `ghcr.io/egao1980/cl-systems/cl-stack-http:0.1.1`

## Quick start

```lisp
(ql:quickload :cl-stack-http) ; or asdf:load-system

(stack-http:with-backend (:dexador)          ; or :winhttp / :async / :auto
  (stack-http:with-session (s :base-url "https://httpbin.org")
    (stack-http:session-get s "/get")
    (stack-http:session-post s "/post" :json '(("a" . 1)))
    (stack-http:response-json
     (stack-http:session-get s "/json"))))

;; pathlib upload / download
(stack-http:with-backend (:dexador)
  (stack-http:download "https://example.com/x.bin" #p"/tmp/x.bin" :overwrite t)
  (stack-http:upload #p"/tmp/x.bin" "https://httpbin.org/post" :as :files))

;; streaming
(stack-http:with-backend (:dexador)
  (stack-http:with-stream (r :get "https://example.com/")
    (stack-http:map-response-lines r #'print)))
```

## Backend selection

| Prefer | System | Notes |
|--------|--------|-------|
| `:auto` | platform order | Windows: winhttp → async → dexador; else dexador → async |
| `:dexador` | `http-backend-dexador` | sync default |
| `:async` | `http-backend-async` | soft-loads `event-backend-libuv` when present |
| `:winhttp` | `http-backend-winhttp` | Windows primary |

```lisp
(stack-http:ensure-http-backend :auto)
(stack-http:with-backend (:winhttp) …)
```

## Serdes

On load, installs yason JSON + readable S-exp codecs into `http-protocol`
`*json-encoder*` / `*data-serializers*`.

```lisp
(stack-http:post url :json ht)                 ; httpx json=
(stack-http:response-json res)
(stack-http:response-text res)                 ; charset-aware

(stack-http:post url :data '(1 2 3) :data-type :sexp)
(http-protocol:response-data res :sexp)
```

## Path / files

Wire bodies stay FS-free (`http-file`). This package bridges pathlib:

- `path-http-file` / `coerce-files` — path → `http-file` (MIME via trivial-mimes)
- httpx file tuples: `("name.txt" octets "text/plain")`
- `:slurp :auto` — memory on dexador, stream otherwise
- `download` / `upload` (+ `-async`) — GET/POST with pathlib write/read

## Session / auth / env

- default timeout **5s**, `:trust-env t` → system/env proxy + `~/.netrc`
- `:auth (digest-auth user pass)` — 401 Digest challenge retry (sync)
- `:cert` → client cert paths on request `:extras`
- `close-session` / `with-session` clears pool

## Layering

| Layer | Role |
|-------|------|
| `http-protocol` + backends | urllib3 / httpx (wire) |
| **cl-stack-http** | requests-like DX |

## License

MIT
