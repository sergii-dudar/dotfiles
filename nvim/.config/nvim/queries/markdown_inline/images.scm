; extends
; Reference-style markdown images for snacks.image: `![alt][label]` (full) and `![label][]` / `![label]` (collapsed,
; shortcut) whose URL sits in a `[label]: url` definition elsewhere in the file. The definitions live in the block-level
; markdown tree, which an inline query cannot see, so the label goes out as "mdref:<label>" and the snacks `resolve` hook
; (lua/plugins/snacks/configs/image-refs.lua) looks it up in the buffer. Snacks' own query (link_destination, wikilink
; shortcut_link) stays as is. A label without a definition falls back to the label as a path.

(image
  (image_description)
  (link_label) @image.src
  (#gsub! @image.src "^%[(.*)%]$" "mdref:%1")) @image

; image_description as the only child: no (link_label), no (link_destination). The predicates sit outside the node so
; the anchors hold. The node text has no outer brackets; ![[x]] wikilinks (text "[x]") are left to snacks' own pattern.
((image
  .
  (image_description) @image.src
  .) @image
  (#not-lua-match? @image.src "^%[")
  (#gsub! @image.src "^(.*)$" "mdref:%1"))