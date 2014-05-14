#ifndef HASKELL_NCURSES_MAVERICKS_C2HS_WORKAROUND_H
#define HASKELL_NCURSES_MAVERICKS_C2HS_WORKAROUND_H

/**
 * OS X 10.9 (Mavericks) has some fancy macros in its <string.h> that prevent
 * c2hs from parsing it. If we define those macros early to have no effect,
 * c2hs is able to proceed.
**/

#ifdef __APPLE__
#define _ANSI_SOURCE
#define __AVAILABILITY__
#define __OSX_AVAILABLE_STARTING(_mac, _iphone)
#define __OSX_AVAILABLE_BUT_DEPRECATED(_macIntro, _macDep, _iphoneIntro, _iphoneDep)
#endif

#include <string.h>

#endif
