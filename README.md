# cl-stack-http

requests / httpx-shaped HTTP client for Common Lisp — thin DX layer over
[`http-protocol`](https://github.com/egao1980/http-protocol) with platform-selectable
backends, JSON + S-expression serdes, and
[`cl-stack-pathlib`](https://github.com/egao1980/cl-stack-pathlib) upload/download.

Package: `cl-stack-http` (nick `stack-http`). **OCI: 0.2.0.**

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
(cl-repo:load-system "cl-stack-http" :version "0.2.0")
;; optional CE: http-encoding-chipz / http-encoding-brotli / http-encoding-zstd
```

OCI: `ghcr.io/egao1980/cl-systems/cl-stack-http:0.2.0`

Pins often used with this facade: `http-protocol` **0.4.3**, backends
`http-backend-async` **0.2.28** + `event-backend-libuv` **0.2.3** /
`http-backend-dexador` **0.1.5** / `http-backend-winhttp` **0.1.2**.

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
| `:auto` | platform order | Windows: winhttp → async → dexador; elsewhere async → dexador. Async counts only when it has an event backend (libuv installed, or one you bound) |
| `:dexador` | `http-backend-dexador` | sync HTTP/1.1 |
| `:async` | `http-backend-async` | event-protocol; HTTP/2 when `http-protocol` asks |
| `:winhttp` | `http-backend-winhttp` | Windows; HTTP/2 via WinHTTP |

```lisp
(stack-http:ensure-http-backend :auto)
(stack-http:with-backend (:winhttp) …)
```

`:async` (explicit or picked by `:auto`) needs no event-loop setup: it loads
`event-backend-libuv` when installed and, when they are unset, sets
`event-protocol:*event-backend*` and `http-backend-async:*event-backend-maker*`.
The maker returns the bound `*event-backend*`, so a libev backend you bind is
used instead. Your own maker or `*event-backend*` is never replaced. A
blocking call makes a loop for itself and closes it; bind
`event-protocol:*event-loop*` to run requests on your own loop.

A backend system is ASDF-loaded once: later requests do not re-plan it
(call `asdf:load-system` yourself to pick up edited sources).

## Responses and conditions

One package for the requests-shaped surface: the `http-protocol` response
readers and conditions are re-exported (the same symbols), so
`stack-http:response-status` reads with a single colon.

| Kind | Symbols |
|------|---------|
| Response | `http-response` `response-status` `response-headers` `response-header` `response-body` `response-data` `response-url` `response-http-version` `response-cookies` `response-history` `response-request` `response-elapsed` `response-bytes-downloaded` `response-trailers` `body-stream` |
| DX | `response-text` `response-json` `response-content` `response-ok-p` `raise-for-status` `with-stream` `iter-lines` … |
| Conditions | `http-error` `http-protocol-error` `http-connection-error` `http-timeout-error` `http-tls-error` `http-redirect-error` `http-canceled` `http-incomplete-response` `http-status-error` (`http-status-error-status`, `http-status-error-response`) `http-client-error` `http-server-error` |
| Transport | `make-http-timeout` `make-http-retry` `make-secret` `reveal` `*http-backend*` |

```lisp
(handler-case (stack-http:get "https://httpbin.org/status/404" :raise-for-status t)
  (stack-http:http-status-error (e)
    (stack-http:http-status-error-status e)))      ; => 404
```

## Serdes

On load, installs [`json-protocol`](https://github.com/egao1980/json-protocol)
(jzon default) + readable S-exp codecs into `http-protocol`
`*json-encoder*` / `*data-serializers*`. JSON `null` → `:null`.

```lisp
(stack-http:post url :json ht)                 ; httpx json=
(stack-http:response-json res)
(stack-http:response-text res)                 ; charset-aware

(stack-http:post url :data '(1 2 3) :data-type :sexp)
(http-protocol:response-data res :sexp)
```

## Path / files

Wire bodies stay FS-free (`http-file`). This package bridges pathlib:

- `path-http-file` / `coerce-files` — path → `http-file` (MIME via mime-protocol)
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

The test workflow runs cl-repository's `test-system.yml`. It installs and
preloads `http-backend-dexador`, `http-backend-async` and
`event-backend-libuv`, and the test-op fails when Rove reports a failure.
Source publish to GHCR runs from this repo:

```bash
gh workflow run publish-checkout.yml -R egao1980/cl-stack-http
```

## License

MIT
