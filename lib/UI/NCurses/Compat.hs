{-# LANGUAGE CPP #-}

-- Copyright (C) 2014 John Millikin <jmillikin@gmail.com>
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

-- This is a separate file because c2hs doesn't define MIN_VERSION_*
-- macros when it's preprocessing a .chs file.
module UI.NCurses.Compat
	( void
	) where

#if MIN_VERSION_base(4,3,0)
import           Control.Monad (void)
#else
void :: Functor f => f a -> f ()
void = fmap (const ())
#endif
