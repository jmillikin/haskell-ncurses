-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | This module is a hack to work around c2hs's lack of #define hooks.
-- the idea is to create a new class 'Enum' which uses 'Integer' instead
-- of 'Int', so integral defines of any size can be retrieved.
module UI.NCurses.Enums where

import           Prelude (Integer, error, show, (++))

#define NCURSES_ENABLE_STDBOOL_H 0
#define _XOPEN_SOURCE_EXTENDED
#define NCURSES_NOMACROS
#include <ncursesw/curses.h>

class Enum a where
	toEnum :: Integer -> a
	fromEnum :: a -> Integer

-- misc enums
#c
enum hsncurses_EnumWrapper
{ hsncurses_ALL_MOUSE_EVENTS = ALL_MOUSE_EVENTS
, hsncurses_ERR = ERR
};
#endc

{# enum hsncurses_EnumWrapper as EnumWrapper {} with prefix = "hsncurses_" #}

-- attributes
#c
enum hsncurses_Attribute
{ hsncurses_WA_STANDOUT = WA_STANDOUT
, hsncurses_WA_UNDERLINE = WA_UNDERLINE
, hsncurses_WA_REVERSE = WA_REVERSE
, hsncurses_WA_BLINK = WA_BLINK
, hsncurses_WA_DIM = WA_DIM
, hsncurses_WA_BOLD = WA_BOLD
, hsncurses_WA_ALTCHARSET = WA_ALTCHARSET
, hsncurses_WA_INVIS = WA_INVIS
, hsncurses_WA_PROTECT = WA_PROTECT
};
#endc

{# enum hsncurses_Attribute as Attribute {} with prefix = "hsncurses_" #}

-- colors
#c
enum hsncurses_Color
{ hsncurses_COLOR_BLACK = COLOR_BLACK
, hsncurses_COLOR_RED = COLOR_RED
, hsncurses_COLOR_GREEN = COLOR_GREEN
, hsncurses_COLOR_YELLOW = COLOR_YELLOW
, hsncurses_COLOR_BLUE = COLOR_BLUE
, hsncurses_COLOR_MAGENTA = COLOR_MAGENTA
, hsncurses_COLOR_CYAN = COLOR_CYAN
, hsncurses_COLOR_WHITE = COLOR_WHITE
};
#endc

{# enum hsncurses_Color as Color {} with prefix = "hsncurses_" #}

-- keys
#c
enum hsncurses_Key
{ hsncurses_KEY_CODE_YES = KEY_CODE_YES
, hsncurses_KEY_DOWN = KEY_DOWN
, hsncurses_KEY_UP = KEY_UP
, hsncurses_KEY_LEFT = KEY_LEFT
, hsncurses_KEY_RIGHT = KEY_RIGHT
, hsncurses_KEY_HOME = KEY_HOME
, hsncurses_KEY_BACKSPACE = KEY_BACKSPACE
, hsncurses_KEY_F0 = KEY_F0
, hsncurses_KEY_DL = KEY_DL
, hsncurses_KEY_IL = KEY_IL
, hsncurses_KEY_DC = KEY_DC
, hsncurses_KEY_IC = KEY_IC
, hsncurses_KEY_EIC = KEY_EIC
, hsncurses_KEY_CLEAR = KEY_CLEAR
, hsncurses_KEY_EOS = KEY_EOS
, hsncurses_KEY_EOL = KEY_EOL
, hsncurses_KEY_SF = KEY_SF
, hsncurses_KEY_SR = KEY_SR
, hsncurses_KEY_NPAGE = KEY_NPAGE
, hsncurses_KEY_PPAGE = KEY_PPAGE
, hsncurses_KEY_STAB = KEY_STAB
, hsncurses_KEY_CTAB = KEY_CTAB
, hsncurses_KEY_CATAB = KEY_CATAB
, hsncurses_KEY_ENTER = KEY_ENTER
, hsncurses_KEY_PRINT = KEY_PRINT
, hsncurses_KEY_LL = KEY_LL
, hsncurses_KEY_A1 = KEY_A1
, hsncurses_KEY_A3 = KEY_A3
, hsncurses_KEY_B2 = KEY_B2
, hsncurses_KEY_C1 = KEY_C1
, hsncurses_KEY_C3 = KEY_C3
, hsncurses_KEY_BTAB = KEY_BTAB
, hsncurses_KEY_BEG = KEY_BEG
, hsncurses_KEY_CANCEL = KEY_CANCEL
, hsncurses_KEY_CLOSE = KEY_CLOSE
, hsncurses_KEY_COMMAND = KEY_COMMAND
, hsncurses_KEY_COPY = KEY_COPY
, hsncurses_KEY_CREATE = KEY_CREATE
, hsncurses_KEY_END = KEY_END
, hsncurses_KEY_EXIT = KEY_EXIT
, hsncurses_KEY_FIND = KEY_FIND
, hsncurses_KEY_HELP = KEY_HELP
, hsncurses_KEY_MARK = KEY_MARK
, hsncurses_KEY_MESSAGE = KEY_MESSAGE
, hsncurses_KEY_MOVE = KEY_MOVE
, hsncurses_KEY_NEXT = KEY_NEXT
, hsncurses_KEY_OPEN = KEY_OPEN
, hsncurses_KEY_OPTIONS = KEY_OPTIONS
, hsncurses_KEY_PREVIOUS = KEY_PREVIOUS
, hsncurses_KEY_REDO = KEY_REDO
, hsncurses_KEY_REFERENCE = KEY_REFERENCE
, hsncurses_KEY_REFRESH = KEY_REFRESH
, hsncurses_KEY_REPLACE = KEY_REPLACE
, hsncurses_KEY_RESTART = KEY_RESTART
, hsncurses_KEY_RESUME = KEY_RESUME
, hsncurses_KEY_SAVE = KEY_SAVE
, hsncurses_KEY_SBEG = KEY_SBEG
, hsncurses_KEY_SCANCEL = KEY_SCANCEL
, hsncurses_KEY_SCOMMAND = KEY_SCOMMAND
, hsncurses_KEY_SCOPY = KEY_SCOPY
, hsncurses_KEY_SCREATE = KEY_SCREATE
, hsncurses_KEY_SDC = KEY_SDC
, hsncurses_KEY_SDL = KEY_SDL
, hsncurses_KEY_SELECT = KEY_SELECT
, hsncurses_KEY_SEND = KEY_SEND
, hsncurses_KEY_SEOL = KEY_SEOL
, hsncurses_KEY_SEXIT = KEY_SEXIT
, hsncurses_KEY_SFIND = KEY_SFIND
, hsncurses_KEY_SHELP = KEY_SHELP
, hsncurses_KEY_SHOME = KEY_SHOME
, hsncurses_KEY_SIC = KEY_SIC
, hsncurses_KEY_SLEFT = KEY_SLEFT
, hsncurses_KEY_SMESSAGE = KEY_SMESSAGE
, hsncurses_KEY_SMOVE = KEY_SMOVE
, hsncurses_KEY_SNEXT = KEY_SNEXT
, hsncurses_KEY_SOPTIONS = KEY_SOPTIONS
, hsncurses_KEY_SPREVIOUS = KEY_SPREVIOUS
, hsncurses_KEY_SPRINT = KEY_SPRINT
, hsncurses_KEY_SREDO = KEY_SREDO
, hsncurses_KEY_SREPLACE = KEY_SREPLACE
, hsncurses_KEY_SRIGHT = KEY_SRIGHT
, hsncurses_KEY_SRSUME = KEY_SRSUME
, hsncurses_KEY_SSAVE = KEY_SSAVE
, hsncurses_KEY_SSUSPEND = KEY_SSUSPEND
, hsncurses_KEY_SUNDO = KEY_SUNDO
, hsncurses_KEY_SUSPEND = KEY_SUSPEND
, hsncurses_KEY_UNDO = KEY_UNDO
, hsncurses_KEY_MOUSE = KEY_MOUSE
, hsncurses_KEY_RESIZE = KEY_RESIZE
, hsncurses_KEY_EVENT = KEY_EVENT
};
#endc

{# enum hsncurses_Key as Key {} with prefix = "hsncurses_" #}

-- mouse buttons
#c
enum hsncurses_Button
{ hsncurses_BUTTON_CTRL = BUTTON_CTRL
, hsncurses_BUTTON_SHIFT = BUTTON_SHIFT
, hsncurses_BUTTON_ALT = BUTTON_ALT

, hsncurses_BUTTON1_RELEASED = BUTTON1_RELEASED
, hsncurses_BUTTON1_PRESSED = BUTTON1_PRESSED
, hsncurses_BUTTON1_CLICKED = BUTTON1_CLICKED
, hsncurses_BUTTON1_DOUBLE_CLICKED = BUTTON1_DOUBLE_CLICKED
, hsncurses_BUTTON1_TRIPLE_CLICKED = BUTTON1_TRIPLE_CLICKED

, hsncurses_BUTTON2_RELEASED = BUTTON2_RELEASED
, hsncurses_BUTTON2_PRESSED = BUTTON2_PRESSED
, hsncurses_BUTTON2_CLICKED = BUTTON2_CLICKED
, hsncurses_BUTTON2_DOUBLE_CLICKED = BUTTON2_DOUBLE_CLICKED
, hsncurses_BUTTON2_TRIPLE_CLICKED = BUTTON2_TRIPLE_CLICKED

, hsncurses_BUTTON3_RELEASED = BUTTON3_RELEASED
, hsncurses_BUTTON3_PRESSED = BUTTON3_PRESSED
, hsncurses_BUTTON3_CLICKED = BUTTON3_CLICKED
, hsncurses_BUTTON3_DOUBLE_CLICKED = BUTTON3_DOUBLE_CLICKED
, hsncurses_BUTTON3_TRIPLE_CLICKED = BUTTON3_TRIPLE_CLICKED

, hsncurses_BUTTON4_RELEASED = BUTTON4_RELEASED
, hsncurses_BUTTON4_PRESSED = BUTTON4_PRESSED
, hsncurses_BUTTON4_CLICKED = BUTTON4_CLICKED
, hsncurses_BUTTON4_DOUBLE_CLICKED = BUTTON4_DOUBLE_CLICKED
, hsncurses_BUTTON4_TRIPLE_CLICKED = BUTTON4_TRIPLE_CLICKED

#ifdef BUTTON5_RELEASED
, hsncurses_BUTTON5_RELEASED = BUTTON5_RELEASED
, hsncurses_BUTTON5_PRESSED = BUTTON5_PRESSED
, hsncurses_BUTTON5_CLICKED = BUTTON5_CLICKED
, hsncurses_BUTTON5_DOUBLE_CLICKED = BUTTON5_DOUBLE_CLICKED
, hsncurses_BUTTON5_TRIPLE_CLICKED = BUTTON5_TRIPLE_CLICKED
#endif
};
#endc

{# enum hsncurses_Button as Button {} with prefix = "hsncurses_" #}
