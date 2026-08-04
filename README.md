# cl-stack-http

requests / httpx-shaped HTTP client for Common Lisp — thin DX layer over
[`http-protocol`](https://github.com/egao1980/http-protocol) with platform-selectable
backends, JSON + S-expression serdes, and
[`cl-stack-pathlib`](https://github.com/egao1980/cl-stack-pathlib) upload/download.

Package: `cl-stack-http` (nick `stack-http`). **OCI: 0.1.7.**

## Layering (Python-shaped)

| Concern | Package |
|---------|---------|
| Wire client (urllib3 / httpx) | [`http-protocol`](https://github.com/egao1980/http-protocol) + backends |
| requests-like DX | **this package** |
| Wire `:basic` / `:bearer`, Digest, netrc, CLOS auth protocol | **this package** |
| OAuth2 get/refresh/scopes/PKCE/401 retry | [`cl-stack-oauth2`](https://github.com/egao1980/cl-stack-oauth2) **0.1.0** |
| JWT encode/decode/verify | [`cl-stack-jwt`](https://github.com/egao1980/cl-stack-jwt) **0.1.0** (jose) |

Auth CLOS protocol: `auth-object-p` / `prepare-auth` / `handle-auth-response`
(`src/auth-protocol.lisp`). Request/response hooks: `prepare-request` / `handle-response`
via `:client-class`.

Cookbook: [`cl-stack` docs/cookbooks/http-client.md](https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/http-client.md).

## Install

```lisp
(cl-repo:load-system "cl-stack-http" :version "0.1.7")
;; optional CE: http-encoding-chipz / http-encoding-brotli / http-encoding-zstd
```

OCI: `ghcr.io/egao1980/cl-systems/cl-stack-http:0.1.7`

Pins often used with this facade: `http-protocol` **0.3.0**, backends
`http-backend-async` **0.2.0** / `http-backend-dexador` **0.1.2** /
`http-backend-winhttp` **0.1.2**.

## Quick start

```lisp
(stack-http:with-backend (:auto)          ; :dexador / :async / :winhttp
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
| `:auto` | platform order | Windows: winhttp → async → dexador; else async/libuv preferred when present |
| `:dexador` | `http-backend-dexador` | sync HTTP/1.1 |
| `:async` | `http-backend-async` | event-protocol; HTTP/2 when `http-protocol` asks |
| `:winhttp` | `http-backend-winhttp` | Windows; HTTP/2 via WinHTTP |

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
- `download` / `download-many` / `upload` (+ `-async`) — streamed GET/POST with pathlib write/read

## Session / auth / env

- default timeout **5s**, `:trust-env t` → system/env proxy + `~/.netrc`
- `:auth '(:basic u p)` / `:auth '(:bearer tok)` — wire (http-protocol)
- `:auth (digest-auth user pass)` — 401 Digest challenge retry (sync)
- `:auth oauth2-auth` — load [`cl-stack-oauth2`](https://github.com/egao1980/cl-stack-oauth2)
- `:cert` → client cert paths on request `:extras`
- `response-elapsed` / `response-bytes-downloaded` after send (0.1.6+)
- `close-session` / `with-session` clears pool

## CLOS hooks

requests `hooks=` / httpx `event_hooks=` → specialize on a client mixin:

```lisp
(defclass logging-client (http-client) ())
(defmethod prepare-request ((c logging-client) request) … request)
(defmethod handle-response ((c logging-client) request response) … response)

(stack-http:with-session (s :preferred :async :client-class 'logging-client …)
  (stack-http:session-get s "/get"))
```

`send` / `send-async` `:around` invokes these for every `http-client`.
Auth stays on `prepare-auth` / `handle-auth-response`.

## Publish

Source-only OCI publish is centralized in [`cl-stack-systems`](https://github.com/egao1980/cl-stack-systems)
(`imports/cl-stack-http/qlfile` pin + shared `publish.yml`). Packaging metadata lives in the `.asd`
(`auto-package-spec`):

```bash
gh workflow run publish.yml -R egao1980/cl-stack-systems -f import=cl-stack-http
```

## License

MIT
