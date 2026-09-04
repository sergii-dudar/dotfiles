# translator.py (vendored)

Command-line translation engine used by [`../translate.sh`](../translate.sh).

- **Upstream:** [skywind3000/translator](https://github.com/skywind3000/translator) @ `d079730`
- **Vendored on:** 2026-09-04

It arrived here as the `engine/` git submodule of [sainnhe/tmux-translator](https://github.com/sainnhe/tmux-translator).
That plugin is unmaintained, and because the Python lives in a *separate* repo behind a
submodule, forking the plugin cannot fix engine bugs. So the one file we actually use is
vendored instead, and `set -g @plugin 'sainnhe/tmux-translator'` is gone from `.tmux.conf`.

Upstream ships no LICENSE file; the code is Copyright (c) skywind3000.

## Local patches

Google's response has `null` at several indices for many language pairs (and for any
invalid language code). Upstream iterates them unguarded, so a bad `--to` produced
`TypeError: 'NoneType' object is not iterable` — which, inside a `tmux popup -E`, just
looked like "the translator is broken".

Guards added (additive only, no upstream code removed) in `GoogleTranslator`:

| method | guard |
|---|---|
| `get_phonetic` | `obj` / `obj[0]` null |
| `get_definition` | `obj` / `obj[0]` null |
| `get_explain` | `obj` null |
| `get_detail` | `resp[12]` null |
| `get_alternative` | `resp[5]` null, and `x[2]` missing/null |

### No `requests` dependency

Upstream's `BasicTranslator.request` needs the third-party `requests` package. On macOS
no python3 has it, and there is no easy way to add it: `/usr/bin/python3` (3.9) carries no
third-party packages at all, Homebrew's is PEP 668 "externally managed" so
`pip3 install --user requests` refuses, and there is no `python-requests` formula. A venv
would work but has to be bootstrapped per machine, which a tmux keybinding should not need.

So `request()` now delegates to `_request_stdlib()`, a urllib implementation of the same
call, returning a `StdlibResponse` that carries the only surface the engines touch:
`.text`, `.json()`, `.content`, `.status_code`. It keeps requests' semantics for GET params,
dict-vs-string POST bodies, `timeout`, `proxy` (both the `all_proxy` config and the standard
`http_proxy` / `https_proxy` env vars), the `User-Agent`, and not raising on 4xx/5xx.

It takes that path on **both** platforms, not only where `requests` is missing. macOS never
has `requests` and Arch usually does — something pulls it in transitively — so keying the
decision off "is it importable" would leave the two machines running different HTTP stacks,
which is exactly the bug you do not want to debug over a tmux popup. It also happens that
requests is the stack that trusts certifi rather than the system store, so it is the one
that cannot see a corporate root CA (below).

The upstream requests path is preserved and reachable with `TRANSLATE_HTTP=requests`. Under
TLS inspection it fails with `unable to get local issuer certificate`; python 2 still uses
it unconditionally, as before.

### TLS behind a corporate proxy

Python 3.13 made `VERIFY_X509_STRICT` a default verify flag. Zscaler's intermediate root CA
is not RFC-5280 clean — its CA `basicConstraints` is not marked critical — so on the work
network every request failed with:

```
urllib.error.URLError: <urlopen error [SSL: CERTIFICATE_VERIFY_FAILED] certificate verify
failed: Basic Constraints of CA cert not marked critical>
```

even though the chain is trusted and `curl` accepts it. `_ssl_context()` clears that one
flag and leaves chain + hostname verification on. (Same class of problem as `gh-push.sh`.)

This is applied on both platforms: the flag is a python-version behaviour, not a macOS one,
and it is read through `getattr(ssl, 'VERIFY_X509_STRICT', 0)` so it is a no-op on the
pre-3.13 interpreters (e.g. the system `/usr/bin/python3` 3.9) that never had it.

Line endings were also normalised CRLF -> LF, per `.editorconfig`.

To diff against upstream (`tr -d '\r'` accounts for that normalisation):

```sh
curl -sL https://raw.githubusercontent.com/skywind3000/translator/d079730/translator.py \
    | tr -d '\r' | diff - translator.py
```