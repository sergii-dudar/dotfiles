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

### cdo

```vim
# by matches:
:cdo s/foo/bar/g | update
:cdo s/foo/bar/gc | up

or:

# by whole files once a file
:cfdo %s/foo/bar/g | up
```

```text
additional navigation:

1. Using Neovim's Built-in Navigation
Neovim has built-in commands to move through jump locations:
Ctrl+o: Move to the previous location (similar to "back" in IntelliJ).
Ctrl+i: Move to the next location (similar to "forward" in IntelliJ).

2.
gf - go to file under cursor,
gF - go to file under cursor and to number after : (File.java:20)

3.
:[line number] - got to line number in buffer

4.
cfdo %s/serhii_dudar/just_serhii/g | update | bd
cfdo %s/just_serhii/serhii_dudar/g | update | bd

```
