#include <signal.h>
#include <string.h>

#include "hsncurses-shim.h"

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

int hsncurses_wget_wch(WINDOW *w, wint_t *out) {
	/*
	Haskell's runtime system uses alarm signals to implement thread
	scheduling. These signals can interrupt system calls such as accept().
	ncurses doesn't handle interrupted system calls gracefully, so all
	the alarms can cause wget_wch() to return much earlier than expected.
	
	As a workaround, we block alarms for the duration of wget_wch().
	*/
	int rc;
	sigset_t signal_alarm, old_mask;
	
	sigemptyset(&signal_alarm);
	sigaddset(&signal_alarm, SIGALRM);
	sigaddset(&signal_alarm, SIGVTALRM);
	
	pthread_sigmask(SIG_BLOCK, &signal_alarm, &old_mask);
	rc = wget_wch(w, out);
	pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
	return rc;
}

void hsncurses_init_cchar_t(cchar_t *wch, attr_t attr, wchar_t *chars, size_t chars_len) {
	size_t ii;
	memset(wch, 0, sizeof(cchar_t));
	wch->attr = attr;
	for (ii = 0; ii < chars_len && ii < CCHARW_MAX; ii++) {
		wch->chars[ii] = chars[ii];
	}
}
