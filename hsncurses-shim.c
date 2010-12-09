#define NCURSES_ENABLE_STDBOOL_H 0
#define _XOPEN_SOURCE_EXTENDED
#define NCURSES_NOMACROS
#include <string.h>
#include <ncursesw/curses.h>

#if NCURSES_VERSION_PATCH < 20081122
int _nc_has_mouse();
#endif

int hsncurses_has_mouse()
{
#if NCURSES_VERSION_PATCH >= 20081122
	return has_mouse();
#else
	return _nc_has_mouse();
#endif
}
