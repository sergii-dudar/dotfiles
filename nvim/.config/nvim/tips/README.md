## source % [so]

source (or execute) the current file as a Vimscript file.
If you are editing your init.vim or init.lua file and want to apply changes immediately:

After making changes, run :source %.
The changes will take effect without needing to restart Neovim.

## NVIM_APPNAME=nvim-lazyvim nvim

## Execute normal mode commands on visually selected block (better for me instead macros)

```java
    .serializationInclusion(Include.NON_NULL)
    .featuresToDisable(WRITE_DATES_AS_TIMESTAMPS, FAIL_ON_UNKNOWN_PROPERTIES)
    .featuresToEnable(ACCEPT_CASE_INSENSITIVE_PROPERTIES)
    .visibility(PropertyAccessor.FIELD, Visibility.ANY)
    .visibility(PropertyAccessor.GETTER, Visibility.NONE)
    .visibility(PropertyAccessor.SETTER, Visibility.NONE)
    .visibility(PropertyAccessor.CREATOR, Visibility.NONE)
    .timeZone(TimeZone.getTimeZone("GMT"))
```

```vim
:'<,'>normal df( f)x

or:

:'<,'>normal df( $x
```

will provide result

```java
Include.NON_NULL)
WRITE_DATES_AS_TIMESTAMPS, FAIL_ON_UNKNOWN_PROPERTIES)
ACCEPT_CASE_INSENSITIVE_PROPERTIES)
PropertyAccessor.FIELD, Visibility.ANY)
PropertyAccessor.GETTER, Visibility.NONE)
PropertyAccessor.SETTER, Visibility.NONE)
PropertyAccessor.CREATOR, Visibility.NONE)
TimeZone.getTimeZone("GMT"))

```
