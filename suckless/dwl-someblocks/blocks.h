// Modify this file to change what commands output to your statusbar, and
// recompile using the make command.
static const Block blocks[] = {
    /*Icon*/ /*Command*/ /*Update Interval*/ /*Update Signal*/
    {"MyMem2:", "free -h | awk '/^Mem/ { print $3\"/\"$2 }' | sed s/i//g", 30,
     0},

    {"Issue:", "echo \"<span color='#ffff00'>text</span>\"", 5, 0},
    {"JustDate:", "date '+%b %d (%a) %I:%M%p'", 5, 0},
    // {"Test", "date '+%b %d (%a) %I:%M%p'", 5, 0},
    // {"Issue", "<span color='#ffff00'>text</span>", 5, 0},

    /* Updates whenever "pkill -SIGRTMIN+10 someblocks" is ran */
    /* {"", "date '+%b %d (%a) %I:%M%p'",
       0,		10}, */
};

// sets delimeter between status commands. NULL character ('\0') means no
// delimeter.
static char delim[] = " | ";
static unsigned int delimLen = 5;