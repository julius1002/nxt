{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Robotics.NXT.Internals where

import Control.Monad.State
import Data.Time.Clock.POSIX
import Data.Typeable
import System.Hardware.Serialport
import Control.Monad.Base

import Robotics.NXT.Externals
import Control.Monad.Trans.Control

{-|
Monad which encompasses interface to the NXT brick.
-}
newtype NXT a = NXT (StateT NXTInternals IO a) deriving (Monad, Applicative, MonadIO, Functor, MonadFix, MonadFail, MonadBase IO) -- NXT monad

{-|
A token used for exposed internal functions.
-}
data NXTInternals = NXTInternals {
    nxthandle :: SerialPort, -- a handle of the opened serial port
    address :: Maybe BTAddress,
    modules :: [(ModuleName, ModuleInfo)], -- modules info
    sleeptime :: Maybe Duration, -- sleep time limit in seconds
    lastkeepalive :: Maybe POSIXTime -- last time keep alive has been sent
  } deriving (Typeable)


{-|
MonadBaseControl instance, useful for creating threads.
-}
instance MonadBaseControl IO NXT where
  type StM NXT a = StM (StateT NXTInternals IO) a
  liftBaseWith f = NXT $ liftBaseWith $ \s -> f (s . (\(NXT nxt) -> nxt))
  restoreM = NXT . restoreM


instance Show NXTInternals where
  show = show . typeOf

{-|
Runs a computation in a context of a given 'NXTInternals' token, returning a value and a new token.
-}
runNXT :: NXT a -> NXTInternals -> IO (a, NXTInternals)
runNXT (NXT action) = runStateT action

{-|
Runs a computation in a context of a given 'NXTInternals' token, returning just a new token.
-}
execNXT :: NXT a -> NXTInternals -> IO NXTInternals
execNXT (NXT action) = execStateT action

modifyNXT :: (NXTInternals -> NXTInternals) -> NXT ()
modifyNXT f = NXT (modify f)

getsNXT :: (NXTInternals -> a) -> NXT a
getsNXT f = NXT (gets f)
