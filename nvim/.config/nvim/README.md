# My NEOVIM config.

### TODO LIST   :

     create java cmd tool `java-class-explorer` to exploring java class byte code to request full qualified type names, fields, inner classes, enum values etc
     mapstruct completion support: using `java-class-explorer` write custom `blink-cmp-mapstruct-source`, where depends from `source` or `target` string context (using java treesitter) explore types from method parameters, or return type, fields list. `@Mapping and @ValueMapping (exploring enums)`
     map struct support - implement goto definition separated path items in mapstruct mapping path
     map struct support - performance and speed improvements (especially javap run)
     neotest-java - investigation of issue of not working debugging for some king of prjects using spring-boot
     investigate stevearc/overseer.nvim integration and implement it to replace CRAG666/code_runner.nvim
     better dap and dap ui configuration (explore more convenient ways to debug java code)
     stevearc/overseer.nvim: implement junit test runners by using junit-platform-console-standalone with parsing result xml and diagnostics, qflist and marks support...
     stabilize `module.java.refactor` for fixing batch moved\renamed java files, packages etc.
     big refactoring of `utils` and `modules`

### Screenshots

![neovim.png](../../../screenshots/nvim/neovim.png)

![neovim-grep.png](../../../screenshots/nvim/neovim-grep.png)