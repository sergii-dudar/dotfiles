# This is Matt's i3 Config.

## Dependencies
* i3-Gaps version 4.20+ (Previous versions will not work with this configuration)
* Rofi
* polybar
* Nerd Fonts

## Changing Themes

In order to change themes, you will need to either change the #include-file lines in the polybar and i3 configs by hand or use my i3-ricer script. 

**Note:** I use absolute paths in both places. Make sure you change the paths for your system or nothing will work. 

Because of the way my script works, both my i3 and polybar configs are configured to pull from a theme file. The theme files (in their respective theme directories) are copied to those theme place holder files. So to change a theme by hand, cp the theme's config file to the theme place holder.

`Example: cp themes/onedark.conf theme.conf (for i3)`

`Example: cp polybar/themes/configs/onedark config.ini`

**Also note that the polybar folder in this directory is a symlink, and therefore should not be trusted**

### ToDo

* Add more themes

