; :TSPlaygroundToggle
; :InspectTree

; MapStruct java() expression injection

; "java(buildFirstName(user.getTestFirstName(), SOME_TEXT))"
(
  (element_value_pair
    key: (identifier) @_key
    value: (string_literal
      (string_fragment) @injection.content
    )
  )
  ; (#eq? @_key "expression")
  (#match? @_key "^(expression|conditionExpression|defaultExpression)$")
  ; (#match? @injection.content "^java\\(.*\\)$")
  (#match? @injection.content "^java\\(.*\\)$")
  (#offset! @injection.content 0 5 0 -1)
  (#set! injection.language "java")
  (#set! injection.include-children)
)

; """
; java(buildFirstName(user.getTestFirstName(), SOME_TEXT))
; """
(
  (element_value_pair
    key: (identifier) @_key
    value: (string_literal
      (multiline_string_fragment) @injection.content
    )
  )
  ; (#eq? @_key "expression")
  (#match? @_key "^(expression|conditionExpression|defaultExpression)$")
  (#match? @injection.content "java\\(")
  (#set! injection.language "java")
  (#set! injection.include-children)
)