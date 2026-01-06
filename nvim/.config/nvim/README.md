# My NEOVIM config.

### TODO LIST   :

     stabilize `java-refactor-util.lua` for fixing batch moved\renamed java files, pachages etc.
     create java cmd tool `java-class-explorer` to exploring java class byte code to request full qualified type names, fields, inner classes, enum values etc
     neotest-java - investigate better solution to resolve parametrized tests custom types (maybe based on `java-class-explorer`), to be able to propose better solution to rcasia as FR.
     mapstruct completion support: using `java-class-explorer` write custom `blink-cmp-mapstruct-source`, where depends from `source` or `target` string context (using java treesitter) explore types from method parameters, or return type, fields list. `@Mapping and @ValueMapping (exploring enums)`
     mapstruct `java(...)` expressings support: configure string expressions to behave like java code in current context, instead of just as plain string.

### Screenshots

![neovim.png](../../../screenshots/nvim/neovim.png)

![neovim-grep.png](../../../screenshots/nvim/neovim-grep.png)
