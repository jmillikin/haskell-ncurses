{-# LANGUAGE DeriveDataTypeable #-}

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

module UI.NCurses.Types where

import qualified Control.Applicative as A
import           Control.Exception (Exception, throwIO)
import           Control.Monad (liftM, ap)
import           Control.Monad.Fix (MonadFix, mfix)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Typeable
import qualified Foreign as F
import qualified Foreign.C as F

import qualified UI.NCurses.Enums as E

-- | A small wrapper around 'IO', to ensure the @ncurses@ library is
-- initialized while running.
newtype Curses a = Curses { unCurses :: IO a }

instance Monad Curses where
	return = Curses . return
	m >>= f = Curses (unCurses m >>= unCurses . f)

instance MonadFix Curses where
	mfix f = Curses (mfix (unCurses . f))

instance MonadIO Curses where
	liftIO = Curses

instance Functor Curses where
	fmap = liftM

instance A.Applicative Curses where
	pure = return
	(<*>) = ap

newtype Update a = Update { unUpdate :: ReaderT Window Curses a }

instance Monad Update where
	return = Update . return
	m >>= f = Update (unUpdate m >>= unUpdate . f)

instance MonadFix Update where
	mfix f = Update (mfix (unUpdate . f))

instance Functor Update where
	fmap = liftM

instance A.Applicative Update where
	pure = return
	(<*>) = ap

newtype Window = Window { windowPtr :: F.Ptr Window }

newtype CursesException = CursesException String
	deriving (Show, Typeable)

instance Exception CursesException

checkRC :: String -> F.CInt -> IO ()
checkRC name rc = if toInteger rc == E.fromEnum E.ERR
	then throwIO (CursesException (name ++ ": rc == ERR"))
	else return ()

cToBool :: Integral a => a -> Bool
cToBool 0 = False
cToBool _ = True

cFromBool :: Integral a => Bool -> a
cFromBool False = 0
cFromBool True = 1
