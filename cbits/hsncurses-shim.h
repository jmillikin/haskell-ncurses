#ifndef HASKELL_NCURSES_SHIM_H
#define HASKELL_NCURSES_SHIM_H

#include <wchar.h>

#define NCURSES_ENABLE_STDBOOL_H 0
#define _XOPEN_SOURCE_EXTENDED
#define NCURSES_NOMACROS

#ifdef HSNCURSES_NARROW_HEADER
#include <ncurses.h>
#else
#include <ncursesw/ncurses.h>
#endif

int hsncurses_has_mouse();

int hsncurses_wget_wch(WINDOW *, wint_t *);

void hsncurses_init_cchar_t(cchar_t *wch, attr_t attr, wchar_t *chars, size_t chars_len);

#endif
