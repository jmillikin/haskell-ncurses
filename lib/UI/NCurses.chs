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

module UI.NCurses
	(
	-- * Primary types
	  Curses
	, Update
	, Window
	, CursesException
	
	-- * Initialization
	, runCurses
	, defaultWindow
	
	-- * Window management
	, newWindow
	, closeWindow
	, cloneWindow
	, moveWindow
	, windowPosition
	, resizeWindow
	, windowSize
	, updateWindow
	
	-- ** Copying window content
	, OverlayMode(..)
	, overlay
	, copyWindow
	
	-- ** Virtual windows (pads)
	, Pad
	, newPad
	, closePad
	, updatePad
	
	-- * The cursor
	, moveCursor
	, cursorPosition
	, getCursor
	
	-- * Drawing to the screen
	, render
	, setColor
	, drawString
	, drawText
	, drawGlyph
	, drawBorder
	, drawBox
	, drawLineH
	, drawLineV
	, clear
	, clearLine
	, setBackground
	
	-- * Attributes
	, Attribute (..)
	, setAttribute
	, setAttributes
	
	-- * Colors
	, Color (..)
	, maxColor
	, ColorID
	, supportsColor
	, canDefineColor
	, defineColor
	, queryColor
	, defaultColorID
	, newColorID
	, setColorID
	, maxColorID
	
	-- * Glyphs
	, Glyph (..)
	
	-- ** VT100 drawing glyphs
	, glyphCornerUL
	, glyphCornerLL
	, glyphCornerUR
	, glyphCornerLR
	, glyphTeeL
	, glyphTeeR
	, glyphTeeB
	, glyphTeeT
	, glyphLineH
	, glyphLineV
	, glyphPlus
	, glyphScan1
	, glyphScan9
	, glyphDiamond
	, glyphStipple
	, glyphDegree
	, glyphPlusMinus
	, glyphBullet
	
	-- ** Teletype 5410v1 symbols
	, glyphArrowL
	, glyphArrowR
	, glyphArrowD
	, glyphArrowU
	, glyphBoard
	, glyphLantern
	, glyphBlock
	
	-- ** Other glyphs
	, glyphS3
	, glyphS7
	, glyphNE
	, glyphLTE
	, glyphGTE
	, glyphPi
	, glyphSterling
	
	-- * Event handling
	, Event (..)
	, getEvent
	
	-- ** Keyboard events
	, Key (..)
	
	-- ** Mouse events
	, ButtonState (..)
	, MouseState (..)
	
	-- * Cursor mode
	, CursorMode(CursorInvisible, CursorVisible, CursorVeryVisible)
	, setCursorMode
	
	-- * Error handling
	, tryCurses
	, catchCurses
	, throwCurses
	
	-- * misc
	, setRaw
	, setCBreak
	, setEcho
	, baudrate
	, beep
	, flash
	, hasMouse
	, enclosed
	, screenSize
	, setTouched
	, setRowsTouched
	, setKeypad
	, resizeTerminal
	) where

import           Control.Exception (bracket_, catch, throwIO, try)
import           Control.Monad (when, unless)
import qualified Control.Monad.Trans.Reader as R
import           Data.Char (chr, ord)
import           Data.List (foldl')
import           Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.Text as T
import           Foreign hiding (shift, void)
import           Foreign.C

import qualified UI.NCurses.Enums as E
import           UI.NCurses.Compat
import           UI.NCurses.Types

#include "cbits/mavericks-c2hs-workaround.h"

-- Note: c2hs has a hard time with the ncurses macros, and will choke on
-- waddwstr() if NCURSES_NOMACROS is not defined prior to including
-- ncurses.h in this file.
--
-- Transitive includes from hsncurses-shim.h are not sufficient.
#define NCURSES_ENABLE_STDBOOL_H 0
#define _XOPEN_SOURCE_EXTENDED
#define NCURSES_NOMACROS
#ifdef HSNCURSES_NARROW_HEADER
#include <ncurses.h>
#else
#include <ncursesw/ncurses.h>
#endif

#include "cbits/hsncurses-shim.h"

{# pointer *WINDOW as Window nocode #}
{# pointer *cchar_t as CCharT newtype #}
{# pointer *wchar_t as CWString nocode #}

type AttrT = {# type attr_t #}
type MMaskT = {# type mmask_t #}

-- | Put the terminal in graphical mode, including enabling special keys,
-- colors, and mouse events (if supported).
--
-- After the 'Curses' block has finished running, the terminal is reset
-- to text mode.
runCurses :: Curses a -> IO a
runCurses = bracket_ initCurses {# call endwin #} . unCurses where
	allEvents = fromInteger (E.fromEnum E.ALL_MOUSE_EVENTS)
	initCurses = do
		void {# call initscr #}
		void {# call cbreak #}
		void $ {# call mousemask #} allEvents nullPtr
		hasColor <- {# call has_colors #}
		when (hasColor == 1) $ do
			void {# call start_color #}
			void {# call use_default_colors #}
		stdscr <- peek c_stdscr
		void $ {# call keypad #} (Window stdscr) 1
		void $ {# call meta #} (Window stdscr) 1
		{# call wtimeout #} (Window stdscr) (- 1)

-- | The default window created when @ncurses@ is initialized, also known
-- as @stdscr@.
defaultWindow :: Curses Window
defaultWindow = Curses (Window `fmap` peek c_stdscr)

foreign import ccall "static &stdscr"
	c_stdscr :: Ptr (Ptr Window)

-- | Create a new 'Window', with the given dimensions. To create a
-- fullscreen window, use @'newWindow' 0 0 0 0@.
--
-- When the window is no longer needed, call 'closeWindow'. Windows are not
-- garbage&#x2013;collected, because there&#x2019;s no way to know if
-- they&#x2019;re still in use (as a background, or event source, etc).
newWindow :: Integer -- ^ Rows
          -> Integer -- ^ Columns
          -> Integer -- ^ Begin Y
          -> Integer -- ^ Begin X
          -> Curses Window
newWindow rows cols x y = Curses $ do
	win <- {# call newwin #}
		(fromInteger rows)
		(fromInteger cols)
		(fromInteger x)
		(fromInteger y)
	if windowPtr win == nullPtr
		then throwIO (CursesException "newWindow: newwin() returned NULL")
		else do
			void $ {# call keypad #} win 1
			void $ {# call meta #} win 1
			{# call wtimeout #} win (- 1)
			return win

-- | Close a window, and free all resources associated with it. Once a
-- window has been closed, it is no longer safe to use.
--
-- Note: this computation will not automatically clear the window from the
-- screen.
closeWindow :: Window -> Curses ()
closeWindow win = Curses ({# call delwin #} win >>= checkRC "closeWindow")

-- | Create a separate window, initialised with the state of an existing
-- window.
cloneWindow :: Window -> Curses Window
cloneWindow old = Curses $ do
	win <- {# call dupwin #} old
	if windowPtr win == nullPtr
		then throwIO (CursesException "cloneWindow: dupwin() returned NULL")
		else return win

-- | Apply a window update to the window. After all of an
-- application&#x2019;s windows have been updated, call 'render' to update
-- the terminal&#x2019;s contents.
updateWindow :: Window -> Update a -> Curses a
updateWindow win (Update reader) = do
	a <- R.runReaderT reader win
	Curses ({# call wnoutrefresh #} win >>= checkRC "updateWindow")
	return a

-- | Moves the window to the given (row,column) coordinate.
moveWindow :: Integer -> Integer -> Update ()
moveWindow row col = withWindow_ "moveWindow"  $ \win ->
	{# call mvwin #} win (fromInteger row) (fromInteger col)

-- | Returns the current (row, column) coordinates of the window.
windowPosition :: Update (Integer, Integer)
windowPosition = withWindow $ \win -> do
	row <- {# call getbegy #} win
	col <- {# call getbegx #} win
	return (toInteger row, toInteger col)

-- | Resizes the window to the given row and column dimensions.
resizeWindow :: Integer -> Integer -> Update ()
resizeWindow rows cols = withWindow_ "resizeWindow"  $ \win ->
	{# call wresize #} win (fromInteger rows) (fromInteger cols)

-- Returns the current (row, column) dimensions of the window.
windowSize :: Update (Integer, Integer)
windowSize = withWindow $ \win -> do
	rows <- {# call getmaxy #} win
	cols <- {# call getmaxx #} win
	return (toInteger rows, toInteger cols)

data OverlayMode
	-- | Overlay only non-blank characters.
	= OverlayMerge
	-- | Overlay all characters, including blanks.
	| OverlayReplace
	deriving (Show, Eq)

-- | Overlay the entire content of another window onto this window.
--
-- The overlay mode specifies whether to copy blank characters.
--
-- Use 'copyWindow' if precise control over coordinates is required.
overlay :: Window -> OverlayMode -> Update ()
overlay src mode = withWindow_ "overlay" $ \dst -> case mode of
	OverlayMerge -> {# call overlay as c_overlay #} src dst
	OverlayReplace -> {# call overwrite #} src dst

-- | Overlay a region of another window onto this window.
--
-- Use 'overlay' for copying the entire area of a window.
copyWindow :: Window
           -> OverlayMode -- Whether to copy blank characters.
           -> Integer -- Top-most row of the source window's overlay region (sminrow).
           -> Integer -- Left-most column of the source window's overlay region (smincol).
           -> Integer -- Top-most row of the destination window's overlay region (dminrow).
           -> Integer -- Left-most column of the destination window's overlay region (dmincol).
           -> Integer -- Bottom-most row of the destination window's overlay region (dmaxrow).
           -> Integer -- Right-most column of the destination window's overlay region (dmaxcol).
           -> Update ()
copyWindow src mode sminrow smincol dminrow dmincol dmaxrow dmaxcol = withWindow_ "copyWindow" $ \dst -> do
	{# call copywin #} src dst
		(fromInteger sminrow)
		(fromInteger smincol)
		(fromInteger dminrow)
		(fromInteger dmincol)
		(fromInteger dmaxrow)
		(fromInteger dmaxcol)
		(cFromBool (mode /= OverlayReplace))

-- | A Pad is a 'Window' that is not associated with the screen.
newtype Pad = Pad Window

-- | Create a new 'Pad' with the given dimensions.
--
-- When the pad is no longer needed, call 'closePad'. Pads are not
-- garbage&#x2013;collected, because there&#x2019;s no way to know if
-- they&#x2019;re still in use.
newPad :: Integer -- ^ Rows
       -> Integer -- ^ Columns
       -> Curses Pad
newPad rows cols = Curses $ do
	win <- {# call newpad #}
		(fromInteger rows)
		(fromInteger cols)
	if windowPtr win == nullPtr
		then throwIO (CursesException "newPad: newpad() returned NULL")
		else do
			void $ {# call keypad #} win 1
			void $ {# call meta #} win 1
			{# call wtimeout #} win (- 1)
			return (Pad win)

-- | Close a pad, and free all resources associated with it. Once a
-- pad has been closed, it is no longer safe to use.
closePad :: Pad -> Curses ()
closePad (Pad win) = Curses ({# call delwin #} win >>= checkRC "closePad")

updatePad :: Pad
          -> Integer -- Top-most row of the pad's update region (pminrow).
          -> Integer -- Left-most column of the pad's update region (pmincol).
          -> Integer -- Top-most row of the screen's update region (sminrow).
          -> Integer -- Left-most column of the screen's update region (smincol).
          -> Integer -- Bottom-most row of the screen's update region (smaxrow).
          -> Integer -- Right-most column of the screen's update region (smaxcol).
          -> Update a
          -> Curses a
updatePad (Pad win) pminrow pmincol sminrow smincol smaxrow smaxcol (Update reader) = do
	a <- R.runReaderT reader win
	Curses $
		({# call pnoutrefresh #} win
			(fromInteger pminrow)
			(fromInteger pmincol)
			(fromInteger sminrow)
			(fromInteger smincol)
			(fromInteger smaxrow)
			(fromInteger smaxcol))
		>>= checkRC "updatePad"
	return a

-- | Move the window&#x2019;s cursor position to the given row and column.
moveCursor :: Integer -- ^ Row
           -> Integer -- ^ Column
           -> Update ()
moveCursor row col = withWindow_ "moveCursor" $ \win ->
	{# call wmove #} win (fromInteger row) (fromInteger col)

-- | Returns the current (row,column) coordinates of the cursor.
--
-- This is the same as 'getCursor', but is usable within an Update.
cursorPosition :: Update (Integer, Integer)
cursorPosition = withWindow $ \win -> do
	row <- {# call getcury #} win
	col <- {# call getcurx #} win
	return (toInteger row, toInteger col)

-- | Return current cursor position as (row, column).
--
-- This is the same as 'cursorPosition', but is usable outside
-- of an Update.
getCursor :: Window -> Curses (Integer, Integer)
getCursor win = Curses $ do
	row <- {# call getcury #} win
	col <- {# call getcurx #} win
	return (toInteger row, toInteger col)

-- | Re&#x2013;draw any portions of the screen which have changed since the
-- last render.
render :: Curses ()
render = Curses ({# call doupdate #} >>= checkRC "render")

-- | Set the current foreground and background colors. See 'newColorID'
-- for how to create color IDs.
setColor :: ColorID -> Update ()
setColor (ColorID pair) = withWindow_ "setColor" $ \win ->
	{# call wcolor_set #} win pair nullPtr

-- | Add some text to the window, at the current cursor position.
drawString :: String -> Update ()
drawString str = withWindow_ "drawString" $ \win ->
	withCWString str ({# call waddwstr #} win)

-- | Add some text to the window, at the current cursor position.
drawText :: T.Text -> Update ()
drawText txt = withWindow_ "drawText" $ \win ->
	withCWString (T.unpack txt) ({# call waddwstr #} win)

drawGlyph :: Glyph -> Update ()
drawGlyph glyph = withWindow_ "drawGlyph" $ \win ->
	withGlyph glyph $ \pGlyph ->
	{# call wadd_wch #} win pGlyph

-- | Draw a border around the edge of the window. For any edge, passing
-- 'Nothing' means to use the default glyph.
drawBorder :: Maybe Glyph -- ^ Left edge
           -> Maybe Glyph -- ^ Right edge
           -> Maybe Glyph -- ^ Top edge
           -> Maybe Glyph -- ^ Bottom edge
           -> Maybe Glyph -- ^ Top left corner
           -> Maybe Glyph -- ^ Top right corner
           -> Maybe Glyph -- ^ Bottom left corner
           -> Maybe Glyph -- ^ Bottom right corner
           -> Update ()
drawBorder le re te be tl tr bl br =
	withWindow_ "drawBorder" $ \win ->
	withMaybeGlyph le $ \pLE ->
	withMaybeGlyph re $ \pRE ->
	withMaybeGlyph te $ \pTE ->
	withMaybeGlyph be $ \pBE ->
	withMaybeGlyph tl $ \pTL ->
	withMaybeGlyph tr $ \pTR ->
	withMaybeGlyph bl $ \pBL ->
	withMaybeGlyph br $ \pBR ->
	{# call wborder_set #} win pLE pRE pTE pBE pTL pTR pBL pBR

-- | @drawBox v h = drawBorder v v h h Nothing Nothing Nothing Nothing@
drawBox :: Maybe Glyph -> Maybe Glyph -> Update ()
drawBox v h = drawBorder v v h h Nothing Nothing Nothing Nothing

-- | Draw a horizontal line from left to right, using the given glyph and
-- maximum character count. The cursor position is not changed.
drawLineH :: Maybe Glyph -> Integer -> Update ()
drawLineH g n = withWindow_ "drawLineH" $ \win ->
	withMaybeGlyph g $ \pChar ->
	{# call whline_set #} win pChar (fromInteger n)

-- | Draw a vertical line from top to bottom, using the given glyph and
-- maximum character count. The cursor position is not changed.
drawLineV :: Maybe Glyph -> Integer -> Update ()
drawLineV g n = withWindow_ "drawLineV" $ \win ->
	withMaybeGlyph g $ \pChar ->
	{# call wvline_set #} win pChar (fromInteger n)

-- | Clear the window content by drawing blanks to every position.
clear :: Update ()
clear = withWindow_ "clear" {# call wclear #}

-- | Clear the current line starting from the current cursor position
-- (inclusive) to the end of the line.
clearLine :: Update ()
clearLine = withWindow_ "clear" {# call wclrtoeol #}

-- | Set the window&#x2019;s background glyph. The glyph will be drawn in
-- place of any blank characters, and the glyph&#x2019;s attributes will be
-- combined with those of every character.
setBackground :: Glyph -> Update ()
setBackground g = withWindow_ "setBackground" $ \win ->
	withMaybeGlyph (Just g) $ \pChar ->
	{# call wbkgrndset #} win pChar >> return 0

data Attribute
	= AttributeColor ColorID -- ^ A_COLOR
	| AttributeStandout -- ^ A_STANDOUT
	| AttributeUnderline -- ^ A_UNDERLINE
	| AttributeReverse -- ^ A_REVERSE
	| AttributeBlink -- ^ A_BLINK
	| AttributeDim -- ^ A_DIM
	| AttributeBold -- ^ A_BOLD
	| AttributeAltCharset -- ^ A_ALTCHARSET
	| AttributeInvisible -- ^ A_INVISIBLE
	| AttributeProtect -- ^ A_PROTECT
	| AttributeHorizontal -- ^ A_HORIZONTAL
	| AttributeLeft -- ^ A_LEFT
	| AttributeLow -- ^ A_LOW
	| AttributeRight -- ^ A_RIGHT
	| AttributeTop -- ^ A_TOP
	| AttributeVertical -- ^ A_VERTICAL
	deriving (Show, Eq)

attrEnum :: E.Attribute -> AttrT
attrEnum = fromInteger . E.fromEnum

attrToInt :: Attribute -> AttrT
attrToInt x = case x of
	AttributeStandout    -> attrEnum E.WA_STANDOUT
	AttributeUnderline   -> attrEnum E.WA_UNDERLINE
	AttributeReverse     -> attrEnum E.WA_REVERSE
	AttributeBlink       -> attrEnum E.WA_BLINK
	AttributeDim         -> attrEnum E.WA_DIM
	AttributeBold        -> attrEnum E.WA_BOLD
	AttributeAltCharset  -> attrEnum E.WA_ALTCHARSET
	AttributeInvisible   -> attrEnum E.WA_INVIS
	AttributeProtect     -> attrEnum E.WA_PROTECT
	AttributeHorizontal  -> attrEnum E.WA_HORIZONTAL
	AttributeLeft        -> attrEnum E.WA_LEFT
	AttributeLow         -> attrEnum E.WA_LOW
	AttributeRight       -> attrEnum E.WA_RIGHT
	AttributeTop         -> attrEnum E.WA_TOP
	AttributeVertical    -> attrEnum E.WA_VERTICAL

	-- Colors get special handling: the function COLOR_PAIR converts an
	-- NCURSES_PAIRS_T to an attr_t.
	AttributeColor (ColorID cid) -> fromIntegral ({# call pure unsafe COLOR_PAIR as c_COLOR_PAIR #} (fromIntegral cid))

-- | Set a single 'Attribute' on the current window. No other attributes
-- are modified.
setAttribute :: Attribute -> Bool -> Update ()
setAttribute attr on = withWindow_ "setAttribute" $ \win ->
	let c = if on then {# call wattr_on #} else {# call wattr_off #} in
	c win (attrToInt attr) nullPtr

-- | Set all 'Attribute's at once on the current window. Any attributes not
-- included in the list will be unset.
setAttributes :: [Attribute] -> Update ()
setAttributes attrs = withWindow_ "setAttributes" $ \win ->
	let cint = foldl' (\acc a -> acc .|. attrToInt a) 0 attrs in
	alloca $ \pPair -> do
	{# call wattr_get #} win nullPtr pPair nullPtr >>= checkRC "setAttributes"
	colorPair <- peek pPair
	{# call wattr_set #} win cint colorPair nullPtr

data Color
	= ColorBlack
	| ColorRed
	| ColorGreen
	| ColorYellow
	| ColorBlue
	| ColorMagenta
	| ColorCyan
	| ColorWhite

	-- | An unspecified default terminal color, for terminals that support
	-- ISO/IEC 6429 escape sequences (or equivalent).
	--
	-- This is most useful for terminals with translucent backgrounds.
	| ColorDefault

	-- | A color outside of the standard COLOR_* enum space, for terminals
	-- that support more than eight colors.
	--
	-- Color-related functions may fail if a Color is provided that cannot
	-- be supported by the current terminal. Users are responsible for
	-- checking 'maxColor' when using extended colors.
	| Color Int16
	deriving (Show, Eq)

-- Get the maximum 'Color' supported by the current terminal.
maxColor :: Curses Integer
maxColor = Curses $ do
	count <- toInteger `fmap` peek c_COLORS
	return (count - 1)

foreign import ccall "static &COLORS"
	c_COLORS :: Ptr CInt


-- | A wrapper around 'Integer' to ensure clients don&#x2019;t use an
-- uninitialized color in an attribute.
newtype ColorID = ColorID CShort
	deriving (Show, Eq)

colorEnum :: E.Color -> CShort
colorEnum = fromInteger . E.fromEnum

colorToShort :: Color -> CShort
colorToShort x = case x of
	Color n      -> CShort n
	ColorBlack   -> colorEnum E.COLOR_BLACK
	ColorRed     -> colorEnum E.COLOR_RED
	ColorGreen   -> colorEnum E.COLOR_GREEN
	ColorYellow  -> colorEnum E.COLOR_YELLOW
	ColorBlue    -> colorEnum E.COLOR_BLUE
	ColorMagenta -> colorEnum E.COLOR_MAGENTA
	ColorCyan    -> colorEnum E.COLOR_CYAN
	ColorWhite   -> colorEnum E.COLOR_WHITE
	ColorDefault -> colorEnum E.COLOR_DEFAULT

-- | Check if the terminal supports color. If it doesn&#x2019;t,
-- alternative indicators (such as underlines or bold) should be used.
supportsColor :: Curses Bool
supportsColor = Curses (fmap cToBool {# call has_colors #})

-- | Check if the terminal supports changing color defintiions.
canDefineColor :: Curses Bool
canDefineColor = Curses (fmap cToBool {# call can_change_color #})

-- | Change the definition of an existing color. Use 'canDefineColor' to
-- determine whether changing color values is possible.
defineColor :: Color
            -> Integer -- ^ Red (0 &#x2013; 1000)
            -> Integer -- ^ Green (0 &#x2013; 1000)
            -> Integer -- ^ Blue (0 &#x2013; 1000)
            -> Curses ()
defineColor c r g b = Curses $ do
	rc <- {# call init_color #}
		(colorToShort c)
		(fromInteger r)
		(fromInteger g)
		(fromInteger b)
	checkRC "defineColor" rc

-- | Query the current definition of the given color (see 'defineColor').
-- The returned tuple is (red, green, blue), with values 0 &#x2013; 1000.
queryColor :: Color -> Curses (Integer, Integer, Integer)
queryColor c = Curses $
	alloca $ \pRed ->
	alloca $ \pGreen ->
	alloca $ \pBlue -> do
		rc <- {# call color_content #} (colorToShort c) pRed pGreen pBlue
		checkRC "queryColor" rc
		red <- fmap toInteger (peek pRed)
		green <- fmap toInteger (peek pGreen)
		blue <- fmap toInteger (peek pBlue)
		return (red, green, blue)

-- | The default color ID
defaultColorID :: ColorID
defaultColorID = ColorID 0

-- | Assign a new 'ColorID' to some (foreground, background) color pair.
-- The user may pick which color ID is assigned, but it must be valid. Use
-- 'maxColorID' to determine how many colors the current terminal supports.
newColorID :: Color -- ^ Foreground
           -> Color -- ^ Background
           -> Integer -- ^ A value /n/, such that
                      -- (0 < /n/ &#x2264; 'maxColorID')
           -> Curses ColorID
newColorID fg bg n = Curses $ do
	unless (n > 0) $ throwIO (CursesException "newColorID: n must be > 0")
	maxColor <- unCurses maxColorID
	unless (n <= maxColor) $ throwIO (CursesException "newColorID: n must be <= maxColorID")
	checkRC "newColorID" =<< {# call init_pair #}
		(fromInteger n)
		(colorToShort fg)
		(colorToShort bg)
	return (ColorID (fromInteger n))

-- Change the definition of an existing 'ColorID'
setColorID :: Color -- ^ Foreground
           -> Color -- ^ Background
           -> ColorID -- ^ The 'ColorID' to change
           -> Curses ()
setColorID fg bg (ColorID n) = Curses $
	checkRC "setColorID" =<< {# call init_pair #} n
		(colorToShort fg)
		(colorToShort bg)

-- | Get the maximum color ID supported by the current terminal
maxColorID :: Curses Integer
maxColorID = Curses $ do
	pairs <- toInteger `fmap` peek c_COLOR_PAIRS
	return (pairs - 1)

foreign import ccall "static &COLOR_PAIRS"
	c_COLOR_PAIRS :: Ptr CInt

-- | A glyph is a character, typically spacing, combined with a set of
-- attributes.
data Glyph = Glyph
	{ glyphCharacter :: Char
	, glyphAttributes :: [Attribute]
	}
	deriving (Show, Eq)

withMaybeGlyph :: Maybe Glyph -> (CCharT -> IO a) -> IO a
withMaybeGlyph Nothing io = io (CCharT nullPtr)
withMaybeGlyph (Just g) io = withGlyph g io

withGlyph :: Glyph -> (CCharT -> IO a) -> IO a
withGlyph (Glyph char attrs) io =
	let cAttrs = foldl' (\acc a -> acc .|. attrToInt a) 0 attrs in
	withCWStringLen [char] $ \(cChars, cCharsLen) ->
	allocaBytes {# sizeof cchar_t #} $ \pBuf -> do
	{# call hsncurses_init_cchar_t #} (CCharT pBuf) cAttrs cChars (fromIntegral cCharsLen)
	io (CCharT pBuf)

-- | Upper left corner
glyphCornerUL :: Glyph
glyphCornerUL = Glyph '\x250C' []

-- | Lower left corner
glyphCornerLL :: Glyph
glyphCornerLL = Glyph '\x2514' []

-- | Upper right corner
glyphCornerUR :: Glyph
glyphCornerUR = Glyph '\x2510' []

-- | Lower right corner
glyphCornerLR :: Glyph
glyphCornerLR = Glyph '\x2518' []

-- | Tee pointing right
glyphTeeL :: Glyph
glyphTeeL = Glyph '\x251C' []

-- | Tee pointing left
glyphTeeR :: Glyph
glyphTeeR = Glyph '\x2524' []

-- | Tee pointing up
glyphTeeB :: Glyph
glyphTeeB = Glyph '\x2534' []

-- | Tee pointing down
glyphTeeT :: Glyph
glyphTeeT = Glyph '\x252C' []

-- | Horizontal line
glyphLineH :: Glyph
glyphLineH = Glyph '\x2500' []

-- | Vertical line
glyphLineV :: Glyph
glyphLineV = Glyph '\x2502' []

-- | Large plus or crossover
glyphPlus :: Glyph
glyphPlus = Glyph '\x253C' []

-- | Scan line 1
glyphScan1 :: Glyph
glyphScan1 = Glyph '\x23BA' []

-- | Scan line 9
glyphScan9 :: Glyph
glyphScan9 = Glyph '\x23BD' []

-- | Diamond
glyphDiamond :: Glyph
glyphDiamond = Glyph '\x25C6' []

-- | Stipple, or checker board
glyphStipple :: Glyph
glyphStipple = Glyph '\x2592' []

-- | Degree symbol
glyphDegree :: Glyph
glyphDegree = Glyph '\xb0' []

-- | Plus/minus
glyphPlusMinus :: Glyph
glyphPlusMinus = Glyph '\xb1' []

-- | Bullet
glyphBullet :: Glyph
glyphBullet = Glyph '\xb7' []

-- | Arrow pointing left
glyphArrowL :: Glyph
glyphArrowL = Glyph '\x2190' []

-- | Arrow pointing right
glyphArrowR :: Glyph
glyphArrowR = Glyph '\x2192' []

-- | Arrow pointing down
glyphArrowD :: Glyph
glyphArrowD = Glyph '\x2193' []

-- | Arrow pointing up
glyphArrowU :: Glyph
glyphArrowU = Glyph '\x2191' []

-- | Board of squares
glyphBoard :: Glyph
glyphBoard = Glyph '\x2592' []

-- | Lantern symbol
glyphLantern :: Glyph
glyphLantern = Glyph '\x2603' []

-- | Solid square block
glyphBlock :: Glyph
glyphBlock = Glyph '\x25AE' []

-- | Scan line 3
glyphS3 :: Glyph
glyphS3 = Glyph '\x23BB' []

-- | Scan line 7
glyphS7 :: Glyph
glyphS7 = Glyph '\x23BC' []

-- | Not equal
glyphNE :: Glyph
glyphNE = Glyph '\x2260' []

-- | Less than or equal
glyphLTE :: Glyph
glyphLTE = Glyph '\x2264' []

-- | Greater than or equal
glyphGTE :: Glyph
glyphGTE = Glyph '\x2265' []

-- | Pi
glyphPi :: Glyph
glyphPi = Glyph '\x3c0' []

-- | UK pounds sterling symbol
glyphSterling :: Glyph
glyphSterling = Glyph '\xa3' []

data Event
	= EventCharacter Char
	| EventSpecialKey Key
	| EventMouse Integer MouseState
	| EventResized
	| EventUnknown Integer
	deriving (Show, Eq)

-- | Get the next 'Event' from a given window.
--
-- If the timeout is 'Nothing', @getEvent@ blocks until an event is received.
--
-- If the timeout is specified, @getEvent@ blocks for up to that many
-- milliseconds. If no event is received before timing out, @getEvent@ returns
-- 'Nothing'.
--
-- If the timeout is 0 or less, @getEvent@ will not block at all.
getEvent :: Window
         -> Maybe Integer -- ^ Timeout, in milliseconds
         -> Curses (Maybe Event)
getEvent win timeout = Curses io where
	io = alloca $ \ptr -> do
		{# call wtimeout #} win $ case timeout of
			Nothing -> -1
			Just n | n <= 0 -> 0
			Just n -> fromInteger n
		rc <- {# call hsncurses_wget_wch #} win ptr
		if toInteger rc == E.fromEnum E.ERR
			then return Nothing
			else fmap Just (parseCode ptr rc)

	parseCode ptr rc = do
		code <- toInteger `fmap` peek ptr
		if rc == 0
			then return (charEvent code)
			else if code == E.fromEnum E.KEY_MOUSE
				then mouseEvent
				else if code == E.fromEnum E.KEY_RESIZE
					then return EventResized
					else keyEvent code
	
	charEvent = EventCharacter . chr . fromInteger
	
	mouseEvent = allocaBytes {# sizeof MEVENT #} $ \pEv -> do
		{# call getmouse #} pEv >>= checkRC "getEvent"
		evID <- fmap toInteger ({# get MEVENT->id #} pEv)
		x <- fmap toInteger ({# get MEVENT->x #} pEv)
		y <- fmap toInteger ({# get MEVENT->y #} pEv)
		z <- fmap toInteger ({# get MEVENT->z #} pEv)
		
		mask <- {# get MEVENT->bstate #} pEv
		let state = parseMouseState mask
		
		return (EventMouse evID (state { mouseCoordinates = (x, y, z) }))
	
	codeF0 = E.fromEnum E.KEY_F0
	codeF64 = codeF0 + 64
	keyEvent code = return $ if code >= codeF0 && code <= codeF64
		then EventSpecialKey (KeyFunction (code - codeF0))
		else case M.lookup code keyMap of
			Just key -> EventSpecialKey key
			Nothing -> EventUnknown code

data Key
	= KeyUpArrow
	| KeyDownArrow
	| KeyLeftArrow
	| KeyRightArrow
	| KeyHome
	| KeyBackspace
	| KeyFunction Integer -- ^ Function keys, F0 &#x2013; F64
	| KeyDeleteLine
	| KeyInsertLine
	| KeyDeleteCharacter
	| KeyInsertCharacter
	| KeyEIC -- ^ Sent by rmir or smir in insert mode
	| KeyClear -- ^ Clear screen
	| KeyEOS -- ^ Clear to end of screen
	| KeyEOL -- ^ Clear to end of line
	| KeyScrollForward
	| KeyScrollBackward
	| KeyNextPage
	| KeyPreviousPage
	| KeySetTab
	| KeyClearTab
	| KeyClearAllTabs
	| KeyEnter
	| KeyPrint
	| KeyHomeDown
	| KeyA1 -- ^ Upper left of keypad
	| KeyA3 -- ^ Upper right of keypad
	| KeyB2 -- ^ Center of keypad
	| KeyC1 -- ^ Lower left of keypad
	| KeyC3 -- ^ Lower right of keypad
	| KeyBackTab
	| KeyBegin
	| KeyCancel
	| KeyClose
	| KeyCommand
	| KeyCopy
	| KeyCreate
	| KeyEnd
	| KeyExit
	| KeyFind
	| KeyHelp
	| KeyMark
	| KeyMessage
	| KeyMove
	| KeyNext
	| KeyOpen
	| KeyOptions
	| KeyPrevious
	| KeyRedo
	| KeyReference
	| KeyRefresh
	| KeyReplace
	| KeyRestart
	| KeyResume
	| KeySave
	| KeyShiftedBegin
	| KeyShiftedCancel
	| KeyShiftedCommand
	| KeyShiftedCopy
	| KeyShiftedCreate
	| KeyShiftedDeleteCharacter
	| KeyShiftedDeleteLine
	| KeySelect
	| KeyShiftedEnd
	| KeyShiftedEOL
	| KeyShiftedExit
	| KeyShiftedFind
	| KeyShiftedHelp
	| KeyShiftedHome
	| KeyShiftedInsertCharacter
	| KeyShiftedLeftArrow
	| KeyShiftedMessage
	| KeyShiftedMove
	| KeyShiftedNext
	| KeyShiftedOptions
	| KeyShiftedPrevious
	| KeyShiftedPrint
	| KeyShiftedRedo
	| KeyShiftedReplace
	| KeyShiftedRightArrow
	| KeyShiftedResume
	| KeyShiftedSave
	| KeyShiftedSuspend
	| KeyShiftedUndo
	| KeySuspend
	| KeyUndo
	deriving (Show, Eq)

keyMap :: M.Map Integer Key
keyMap = M.fromList $ map (\(enum, key) -> (E.fromEnum enum, key))
	[ (E.KEY_DOWN, KeyDownArrow)
	, (E.KEY_UP, KeyUpArrow)
	, (E.KEY_LEFT, KeyLeftArrow)
	, (E.KEY_RIGHT, KeyRightArrow)
	, (E.KEY_HOME, KeyHome)
	, (E.KEY_BACKSPACE, KeyBackspace)
	, (E.KEY_DL, KeyDeleteLine)
	, (E.KEY_IL, KeyInsertLine)
	, (E.KEY_DC, KeyDeleteCharacter)
	, (E.KEY_IC, KeyInsertCharacter)
	, (E.KEY_EIC, KeyEIC)
	, (E.KEY_CLEAR, KeyClear)
	, (E.KEY_EOS, KeyEOS)
	, (E.KEY_EOL, KeyEOL)
	, (E.KEY_SF, KeyScrollForward)
	, (E.KEY_SR, KeyScrollBackward)
	, (E.KEY_NPAGE, KeyNextPage)
	, (E.KEY_PPAGE, KeyPreviousPage)
	, (E.KEY_STAB, KeySetTab)
	, (E.KEY_CTAB, KeyClearTab)
	, (E.KEY_CATAB, KeyClearAllTabs)
	, (E.KEY_ENTER, KeyEnter)
	, (E.KEY_PRINT, KeyPrint)
	, (E.KEY_LL, KeyHomeDown)
	, (E.KEY_A1, KeyA1)
	, (E.KEY_A3, KeyA3)
	, (E.KEY_B2, KeyB2)
	, (E.KEY_C1, KeyC1)
	, (E.KEY_C3, KeyC3)
	, (E.KEY_BTAB, KeyBackTab)
	, (E.KEY_BEG, KeyBegin)
	, (E.KEY_CANCEL, KeyCancel)
	, (E.KEY_CLOSE, KeyClose)
	, (E.KEY_COMMAND, KeyCommand)
	, (E.KEY_COPY, KeyCopy)
	, (E.KEY_CREATE, KeyCreate)
	, (E.KEY_END, KeyEnd)
	, (E.KEY_EXIT, KeyExit)
	, (E.KEY_FIND, KeyFind)
	, (E.KEY_HELP, KeyHelp)
	, (E.KEY_MARK, KeyMark)
	, (E.KEY_MESSAGE, KeyMessage)
	, (E.KEY_MOVE, KeyMove)
	, (E.KEY_NEXT, KeyNext)
	, (E.KEY_OPEN, KeyOpen)
	, (E.KEY_OPTIONS, KeyOptions)
	, (E.KEY_PREVIOUS, KeyPrevious)
	, (E.KEY_REDO, KeyRedo)
	, (E.KEY_REFERENCE, KeyReference)
	, (E.KEY_REFRESH, KeyRefresh)
	, (E.KEY_REPLACE, KeyReplace)
	, (E.KEY_RESTART, KeyRestart)
	, (E.KEY_RESUME, KeyResume)
	, (E.KEY_SAVE, KeySave)
	, (E.KEY_SBEG, KeyShiftedBegin)
	, (E.KEY_SCANCEL, KeyShiftedCancel)
	, (E.KEY_SCOMMAND, KeyShiftedCommand)
	, (E.KEY_SCOPY, KeyShiftedCopy)
	, (E.KEY_SCREATE, KeyShiftedCreate)
	, (E.KEY_SDC, KeyShiftedDeleteCharacter)
	, (E.KEY_SDL, KeyShiftedDeleteLine)
	, (E.KEY_SELECT, KeySelect)
	, (E.KEY_SEND, KeyShiftedEnd)
	, (E.KEY_SEOL, KeyShiftedEOL)
	, (E.KEY_SEXIT, KeyShiftedExit)
	, (E.KEY_SFIND, KeyShiftedFind)
	, (E.KEY_SHELP, KeyShiftedHelp)
	, (E.KEY_SHOME, KeyShiftedHome)
	, (E.KEY_SIC, KeyShiftedInsertCharacter)
	, (E.KEY_SLEFT, KeyShiftedLeftArrow)
	, (E.KEY_SMESSAGE, KeyShiftedMessage)
	, (E.KEY_SMOVE, KeyShiftedMove)
	, (E.KEY_SNEXT, KeyShiftedNext)
	, (E.KEY_SOPTIONS, KeyShiftedOptions)
	, (E.KEY_SPREVIOUS, KeyShiftedPrevious)
	, (E.KEY_SPRINT, KeyShiftedPrint)
	, (E.KEY_SREDO, KeyShiftedRedo)
	, (E.KEY_SREPLACE, KeyShiftedReplace)
	, (E.KEY_SRIGHT, KeyShiftedRightArrow)
	, (E.KEY_SRSUME, KeyShiftedResume)
	, (E.KEY_SSAVE, KeyShiftedSave)
	, (E.KEY_SSUSPEND, KeyShiftedSuspend)
	, (E.KEY_SUNDO, KeyShiftedUndo)
	, (E.KEY_SUSPEND, KeySuspend)
	, (E.KEY_UNDO, KeyUndo)
	]

data ButtonState
	= ButtonPressed
	| ButtonReleased
	| ButtonClicked
	| ButtonDoubleClicked
	| ButtonTripleClicked
	deriving (Show, Eq)

data MouseState = MouseState
	{ mouseCoordinates :: (Integer, Integer, Integer) -- ^ (X, Y, Z)
	
	-- | If the mouse event was caused by a change in button state,
	-- the buttons and their new state will be listed here.
	, mouseButtons :: [(Integer, ButtonState)]
	
	, mouseAlt :: Bool
	, mouseShift :: Bool
	, mouseControl :: Bool
	}
	deriving (Show, Eq)

parseMouseState :: MMaskT -> MouseState
parseMouseState mask = MouseState (0, 0, 0) buttons alt shift ctrl where
	maskI = toInteger mask
	test e = (maskI .&. (E.fromEnum e)) > 0
	
	alt = test E.BUTTON_ALT
	shift = test E.BUTTON_SHIFT
	ctrl = test E.BUTTON_CTRL
	
	buttons = catMaybes [button1, button2, button3, button4, button5]
	
	testButton idx r p c dc tc
		| test r  = Just (idx, ButtonReleased)
		| test p  = Just (idx, ButtonPressed)
		| test c  = Just (idx, ButtonClicked)
		| test dc = Just (idx, ButtonDoubleClicked)
		| test tc = Just (idx, ButtonTripleClicked)
		| otherwise = Nothing
	
	button1 = testButton 1
		E.BUTTON1_RELEASED
		E.BUTTON1_PRESSED
		E.BUTTON1_CLICKED
		E.BUTTON1_DOUBLE_CLICKED
		E.BUTTON1_TRIPLE_CLICKED
	button2 = testButton 2
		E.BUTTON2_RELEASED
		E.BUTTON2_PRESSED
		E.BUTTON2_CLICKED
		E.BUTTON2_DOUBLE_CLICKED
		E.BUTTON2_TRIPLE_CLICKED
	button3 = testButton 3
		E.BUTTON3_RELEASED
		E.BUTTON3_PRESSED
		E.BUTTON3_CLICKED
		E.BUTTON3_DOUBLE_CLICKED
		E.BUTTON3_TRIPLE_CLICKED
	button4 = testButton 4
		E.BUTTON4_RELEASED
		E.BUTTON4_PRESSED
		E.BUTTON4_CLICKED
		E.BUTTON4_DOUBLE_CLICKED
		E.BUTTON4_TRIPLE_CLICKED
#ifdef BUTTON5_RELEASED
	button5 = testButton 5
		E.BUTTON5_RELEASED
		E.BUTTON5_PRESSED
		E.BUTTON5_CLICKED
		E.BUTTON5_DOUBLE_CLICKED
		E.BUTTON5_TRIPLE_CLICKED
#else
	button5 = Nothing
#endif

data CursorMode
	= CursorInvisible
	| CursorVisible
	| CursorVeryVisible
	| CursorModeUnknown CInt
	deriving (Eq, Show)

-- | Set the current cursor mode to visible, invisible, or \"very visible\".
-- The previous cursor mode is returned.
setCursorMode :: CursorMode -> Curses CursorMode
setCursorMode mode = Curses $ do
	let intMode = case mode of
		CursorInvisible -> 0
		CursorVisible -> 1
		CursorVeryVisible -> 2
		CursorModeUnknown n -> n
	rc <- {# call curs_set #} intMode
	checkRC "setCursorMode" rc
	return $ case rc of
		0 -> CursorInvisible
		1 -> CursorVisible
		2 -> CursorVeryVisible
		_ -> CursorModeUnknown rc

-- | Returns Left if a Curses exception occured in the given computation.
--
-- See 'try' for more details.
tryCurses :: Curses a -> Curses (Either CursesException a)
tryCurses (Curses io) = Curses (try io)

-- | Handles errors in the given computation by passing them to a callback.
--
-- See 'catch' for more details.
catchCurses :: Curses a -> (CursesException -> Curses a) -> Curses a
catchCurses (Curses io) fn = Curses (catch io (unCurses . fn))

-- | Throws an exception from within Curses handling code. This is useful
-- for re-throwing errors from within a 'catchCurses' callback.
--
-- See 'throwIO' for more details.
throwCurses :: CursesException -> Curses a
throwCurses = Curses . throwIO

-- | Runs @raw()@ or @noraw()@
setRaw :: Bool -> Curses ()
setRaw set = Curses (io >>= checkRC "setRaw") where
	io = if set then {# call raw #} else {# call noraw #}

-- | Runs @cbreak()@ or @nocbreak()@
setCBreak :: Bool -> Curses ()
setCBreak set = Curses (io >>= checkRC "setCBreak") where
	io = if set then {# call cbreak #} else {# call nocbreak #}

-- | Runs @echo()@ or @noecho()@
setEcho :: Bool -> Curses ()
setEcho set = Curses (io >>= checkRC "setEcho") where
	io = if set then {# call echo #} else {# call noecho #}

-- | Get the output speed of the current terminal, in bits per second.
baudrate :: Curses Integer
baudrate = Curses $ do
	rc <- {# call baudrate as c_baudrate #}
	checkRC "baudrate" rc
	return (toInteger rc)

beep :: Curses ()
beep = Curses ({# call beep as c_beep #} >>= checkRC "beep")

flash :: Curses ()
flash = Curses ({# call flash as c_flash #} >>= checkRC "flash")

-- | Check if the terminal has a mouse
hasMouse :: Curses Bool
hasMouse = Curses (fmap cToBool c_hasMouse)

foreign import ccall unsafe "hsncurses_has_mouse"
	c_hasMouse :: IO CInt

-- | Check if some position is contained within the given 'Window'.
enclosed :: Window
         -> Integer -- ^ Row
         -> Integer -- ^ Column
         -> Curses Bool
enclosed win row col = Curses . fmap cToBool $
	{# call wenclose #} win (fromInteger row) (fromInteger col)

-- | Return (rows, columns) of current screen
screenSize :: Curses (Integer, Integer)
screenSize = Curses $ do
	rows <- peek c_LINES
	cols <- peek c_COLS
	return (toInteger rows, toInteger cols)

foreign import ccall "static &LINES"
	c_LINES :: Ptr CInt

foreign import ccall "static &COLS"
	c_COLS :: Ptr CInt

-- | Set whether the entire window has been &#x201C;touched&#x201D;;
-- touched characters are redrawn on the next refresh.
setTouched :: Bool -> Update ()
setTouched touched = withWindow_ "setTouched" $ if touched
	then {# call touchwin #}
	else {# call untouchwin #}

-- | Set whether particular rows in the window have been
-- &#x201C;touched&#x201D;.
setRowsTouched :: Bool
                -> Integer -- ^ Start
                -> Integer -- ^ Count
                -> Update ()
setRowsTouched touched start count = withWindow_ "setRowsTouched" $ \win ->
	{# call wtouchln #} win
		(fromInteger start)
		(fromInteger count)
		(cFromBool touched)

-- | Enable/disable support for special keys.
setKeypad :: Window -> Bool -> Curses ()
setKeypad win set = Curses (io >>= checkRC "setKeypad") where
	io = {# call keypad #} win (cFromBool set)

-- | Attempt to resize the terminal to the given number of lines and columns.
resizeTerminal :: Integer -> Integer -> Curses ()
resizeTerminal lines cols = Curses (io >>= checkRC "resizeTerminal") where
	io = {# call resizeterm #} (fromInteger lines) (fromInteger cols)

withWindow :: (Window -> IO a) -> Update a
withWindow io = Update (R.ReaderT (\win -> Curses (io win)))

withWindow_ :: String -> (Window -> IO CInt) -> Update ()
withWindow_ name io = withWindow $ \win -> io win >>= checkRC name
