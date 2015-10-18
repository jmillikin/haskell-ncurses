{-# LANGUAGE ForeignFunctionInterface #-}

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

module UI.NCurses.Panel
	( Panel
	, newPanel
	, deletePanel
	, refreshPanels
	, panelAbove
	, panelBelow
	, panelTop
	, panelBottom
	, showPanel
	, hidePanel
	, panelHidden
	, movePanel
	, raisePanel
	, lowerPanel
	, getPanelWindow
	, replacePanelWindow
	) where

import           Control.Exception (throwIO)
import           Foreign
import           Foreign.C

import           UI.NCurses (render) -- for haddock
import           UI.NCurses.Types

#include "cbits/mavericks-c2hs-workaround.h"

#ifdef HSNCURSES_NARROW_HEADER
#include <panel.h>
#else
#include <ncursesw/panel.h>
#endif

{# pointer *PANEL as Panel nocode #}
{# pointer *WINDOW as Window nocode #}
newtype Panel = Panel { panelPtr :: Ptr Panel }

-- | Creates a new 'Panel', on top of the panel stack.
newPanel :: Window -> Curses Panel
newPanel win = Curses $ do
	p <- {# call new_panel #} win
	if panelPtr p == nullPtr
		then throwIO (CursesException "newPanel: new_panel() returned NULL")
		else return p

-- | Permanently removes the given panel from the panel stack.
deletePanel :: Panel -> Curses ()
deletePanel p = Curses ({# call del_panel #} p >>= checkRC "deletePanel")

-- | Updates windows to account for the current panel stack order. The user
-- must call 'render' before changes are drawn to the screen.
refreshPanels :: Curses ()
refreshPanels = Curses {#call update_panels #}

-- | @panelAbove p@ retrieve the panel above /p/.
panelAbove :: Panel -> Curses (Maybe Panel)
panelAbove p = Curses $ do
	ptr <- {# call panel_above #} p
	return $ if panelPtr ptr == nullPtr
		then Nothing
		else Just ptr

-- | @panelAbove p@ retrieve the panel below /p/.
panelBelow :: Panel -> Curses (Maybe Panel)
panelBelow p = Curses $ do
	ptr <- {# call panel_below #} p
	return $ if panelPtr ptr == nullPtr
		then Nothing
		else Just ptr

-- | Retrieve the top&#x2013;most panel in the stack.
panelTop :: Curses (Maybe Panel)
panelTop = Curses $ do
	ptr <- {# call panel_below #} (Panel nullPtr)
	return $ if panelPtr ptr == nullPtr
		then Nothing
		else Just ptr

-- | Retrieve the bottom&#x2013;most panel in the stack.
panelBottom :: Curses (Maybe Panel)
panelBottom = Curses $ do
	ptr <- {# call panel_above #} (Panel nullPtr)
	return $ if panelPtr ptr == nullPtr
		then Nothing
		else Just ptr

-- | Makes a hidden panel visible, and places it on the top of the stack.
showPanel :: Panel -> Curses ()
showPanel p = Curses ({# call show_panel #} p >>= checkRC "showPanel")

-- | Temporarily removes the given panel from the panel stack. Use
-- 'showPanel' to restore it.
hidePanel :: Panel -> Curses ()
hidePanel p = Curses ({# call hide_panel #} p >>= checkRC "hidePanel")

-- | Checks if the given panel is currently visible.
panelHidden :: Panel -> Curses Bool
panelHidden p = Curses (cToBool `fmap` {# call panel_hidden #} p)

-- | Move the panel so its upper&#x2013;left corner is at the new
-- coordinates.
movePanel :: Panel
          -> Integer -- ^ New upper&#x2013;left row
          -> Integer -- ^ New upper&#x2013;left column
          -> Curses ()
movePanel p row col = Curses $
	checkRC "movePanel" =<< {# call move_panel #} p
		(fromInteger row)
		(fromInteger col)

-- | Raise a bottom to the top of the stack.
raisePanel :: Panel -> Curses ()
raisePanel p = Curses ({# call top_panel #} p >>= checkRC "raisePanel")

-- | Lower a panel to the bottom of the stack.
lowerPanel :: Panel -> Curses ()
lowerPanel p = Curses ({# call bottom_panel #} p >>= checkRC "lowerPanel")

-- | Retrieves which window a panel is drawn to.
getPanelWindow :: Panel -> Curses Window
getPanelWindow p = Curses ({# call panel_window #} p)

-- | Replaces which window a panel is drawn to.
replacePanelWindow :: Panel -> Window -> Curses ()
replacePanelWindow p win = Curses $
	{# call replace_panel #} p win >>= checkRC "replacePanelWindow"
