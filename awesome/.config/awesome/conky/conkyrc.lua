-----------------------------------------------------------------------------
--                               conkyrc_seamod
-- Date    : 04/23/2016
-- Author  : SeaJey and Maxiwell
-- Conky   : >= 1.10 
-- License : Distributed under the terms of GNU GPL version 2 or later
-----------------------------------------------------------------------------

conky.config = {

	background = true,
	update_interval = 1,
	time_in_seconds = true,

	cpu_avg_samples = 2,
	net_avg_samples = 2,
	temperature_unit = 'farenheight',

	double_buffer = true,
	no_buffers = true,
	text_buffer_size = 2048,

	gap_x = 0,
	gap_y = 150,
	minimum_width = 100, minimum_height = 900,
	maximum_width = 115,

	own_window = true,
	own_window_type = 'desktop',
	own_window_transparent = true,
	own_window_argb_visual = true,
	own_window_class = 'conky-semi',
	own_window_hints = 'undecorated,below,sticky,skip_taskbar,skip_pager',

	border_inner_margin = 0,
	border_outer_margin = 0,
	alignment = 'top_left',


	draw_shades = false,
	draw_outline = false,
	draw_borders = false,
	draw_graph_borders = false,

	override_utf8_locale = true,
	use_xft = true,
	font = 'caviar dreams:size=11',
	xftalpha = 0.5,
	uppercase = false,

-- Defining colors
	default_color = '#FFFFFF',
-- Shades of Gray
	color1 = '#DDDDDD',
	color2 = '#AAAAAA',
	color3 = '#888888',
-- Gentoo Purple
	color4 = '#7A5ADA',
-- Green
	color5 = '#8FEB8F',
-- Red
	color6 = '#F45F45',
-- Loading lua script for drawning rings
	lua_load = '~/.config/awesome/conky/seamod_rings.lua',
	lua_draw_hook_pre = 'main',

};

--${offset 15}${font Droid Sans:size=11:style=normal}${color1}${pre_exec lsb_release -d | cut -f 2} - $sysname $kernel
conky.text = [[

${font Droid Sans:size=8:style=normal}${color1}$kernel
${font Droid Sans:size=8:style=normal}${color1}Temp ${color3}$alignr${exec 10 sensors | grep Core\ 3 | awk '{print $3}'}
${font Droid Sans:size=9:style=normal}${color1}NVidia Optimus: ${color3}$alignr${execi 10 cat /proc/acpi/bbswitch | awk '{print $2}'}
${font Droid Sans:size=9:style=normal}${color1}Uptime: $alignr${color3}${color3}$uptime
#${voffset 40}
#${offset 65}${font Droid Sans:size=16:style=bold}${color5}BAT
#
#${voffset -35}
${font Droid Sans:size=8:style=normal}${color1}Status ${color3}$alignr${battery BAT1}
${font Droid Sans:size=8:style=normal}${color1}Time Left ${font Droid Sans:size=8:bold:style=normal}${color4}$alignr${format_time $battery_time "\hh\mm"}${battery_time BAT1}

# Showing CPU Graph
${voffset 40}
${offset 65}${font Droid Sans:size=19:style=bold}${color5}CPU
${voffset 10}
${cpugraph cpu1 20,118 666666 666666}
${voffset -40}
${font Droid Sans:size=9:style=normal}${color1}CPU Freq:  ${font Droid Sans:size=9:bold:style=normal}${alignr}${color4}${freq} ${color2}MHz

# Showing TOP 5 CPU-consumers
${font Droid Sans:bold:size=8:style=normal}${color4}${top name 1}${alignr}${top cpu 1}%
${font Droid Sans:size=8:style=normal}${color1}${top name 2}${alignr}${top cpu 2}%
${font Droid Sans:size=8:style=normal}${color2}${top name 3}${alignr}${top cpu 3}%
${font Droid Sans:size=8:style=normal}${color3}${top name 4}${alignr}${top cpu 4}%
${font Droid Sans:size=8:style=normal}${color3}${top name 5}${alignr}${top cpu 5}%

#Showing memory part with TOP 5
${voffset 30}
${offset 65}${font Droid Sans:size=14:style=bold}${color5}MEM
${voffset 1}
${font Droid Sans:bold:size=8:style=normal}${color4}${top_mem name 1}${alignr}${top_mem mem_res 1}
${font Droid Sans:size=8:style=normal}${color1}${top_mem name 2}${alignr}${top_mem mem_res 2}
${font Droid Sans:size=8:style=normal}${color2}${top_mem name 3}${alignr}${top_mem mem_res 3}
${font Droid Sans:size=8:style=normal}${color3}${top_mem name 4}${alignr}${top_mem mem_res 4}
${font Droid Sans:size=8:style=normal}${color3}${top_mem name 4}${alignr}${top_mem mem_res 5}

# Showing disk partitions: boot, root, home
${voffset 47}
${offset 70}${font Droid Sans:size=12:style=bold}${color5}DISKS
${voffset 20}
${diskiograph 20,118 666666 666666}${voffset -30}
${voffset 10}
${font Droid Sans:size=8:}${color1}Boot Free: ${alignr}$color3${font Droid Sans:size=8:style=normal}${fs_free /boot}
${font Droid Sans:size=8:}${color1}Root Free: ${alignr}$color3${font Droid Sans:size=8:style=normal}${fs_free /}
${font Droid Sans:size=8:}${color1}Home Free: ${alignr}$color3${font Droid Sans:size=8:style=normal}${fs_free /home}

# Network 
${voffset 49}
${offset 70}${font Droid Sans:size=14:style=bold}${color5}WiFi
${voffset 10}
${font Droid Sans:size=10:style=bold}${color1}${color2}VPN: ${font Droid Sans:size=10:style=bold}${color5}${if_up tun0}UP${else}${color6}Down$endif$font$color
${font Droid Sans:size=8:style=bold}${color1}Lan IP: ${alignr}$color3${addr wlp6s0} 
${font Droid Sans:size=8:style=bold}${color1}Ext IP: ${alignr}${color3}NOPE#${alignr}$color3${execi 600 wget -q -O /dev/stdout http://checkip.dyndns.org/ | cut -d : -f 2- | cut -d \< -f -1} 
#${font Droid Sans:size=8:style=bold}${alignr}$color3${execi 600 wget -q -O /dev/stdout https://www.dnsleaktest.com/ | grep from | grep -o '<p>.*<img' | grep -o '>.*<' | grep -oEi '[a-zA-Z0-9 ,]+'}
${voffset 10}             
${color1}${font Droid Sans:size=8:style=bold}Up: ${alignr}${font Droid Sans:size=8:style=normal}$color2${upspeed wlp6s0} / ${totalup wlp6s0}
${upspeedgraph wlp6s0 40,118 4B1B0C FF5C2B 1280KiB -l}
${color1}${font Droid Sans:size=8:style=bold}Down: ${alignr}${font Droid Sans:size=8:style=normal}$color2${downspeed wlp6s0} / ${totaldown wlp6s0}
${downspeedgraph wlp6s0 40,118 324D23 77B753 1280KiB -l}
]];
