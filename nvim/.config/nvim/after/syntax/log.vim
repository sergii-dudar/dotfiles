" Custom log syntax rules (loaded after log-highlight.nvim)

" You can link to any existing highlight group (ErrorMsg, WarningMsg, Label,
"  Directory, String, Special, etc.) or the plugin's groups (LogLvError, LogLvInfo,
"  etc.)."

" Match anything in square brackets e.g. [ERROR], [main], [2025-03-10]
syn match LogBracketContent display '\[.\{-}\]'
hi def link LogBracketContent LogGreen

" Match ua.raiffeisen package paths
syn match LogRaiffeisen display 'ua\.raiffeisen[[:alnum:]._$]\+'
hi def link LogRaiffeisen LogLvDebug

" ========================================
" JSON blocks
" ========================================
syn region LogJsonBlock start='{' end='}' contains=LogJsonBlock
hi def link LogJsonBlock LogGreen

" ========================================
" Java toString objects (class Foo { ... })
" ========================================
" syn region LogToString start='class \S\+ {' end='}' contains=LogToStringInner
" syn region LogToStringInner start='{' end='}' contained transparent contains=LogToStringInner
" hi def link LogToString LogGreen