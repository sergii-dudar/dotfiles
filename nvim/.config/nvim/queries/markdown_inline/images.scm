; extends
; Reference-style markdown images for snacks.image, see lua/plugins/snacks/configs/image-refs.lua for the two helpers.
; Snacks' own patterns (inline `![](url)`, `![[wikilink]]`) stay as they are.

; ![alt][label]
(image
  (image_description)
  (link_label) @image.src
  (#md-ref-dest! @image.src)) @image

; ![label][] and ![label]: image_description is the only child. The predicates sit outside the node so the anchors hold.
; The node text has no outer brackets; ![[x]] wikilinks (text "[x]") are left to snacks.
((image
  .
  (image_description) @image.src
  .) @image
  (#not-lua-match? @image.src "^%[")
  (#md-ref-dest! @image.src))

; plain links whose definition is an image file: jdtls javadoc hovers render diagrams as `[1]` + `[1]: file:///...svg`
((shortcut_link
  (link_text) @image.src) @image
  (#md-ref-is-image? @image.src)
  (#md-ref-dest! @image.src))

((full_reference_link
  (link_text)
  (link_label) @image.src) @image
  (#md-ref-is-image? @image.src)
  (#md-ref-dest! @image.src))

((collapsed_reference_link
  (link_text) @image.src) @image
  (#md-ref-is-image? @image.src)
  (#md-ref-dest! @image.src))