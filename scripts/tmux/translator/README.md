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

Line endings were also normalised CRLF -> LF, per `.editorconfig`.

To diff against upstream (`tr -d '\r'` accounts for that normalisation):

```sh
curl -sL https://raw.githubusercontent.com/skywind3000/translator/d079730/translator.py \
    | tr -d '\r' | diff - translator.py
```