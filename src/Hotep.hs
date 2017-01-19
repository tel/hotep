
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- |
--
-- Hotep is OTP in Haskell.
--
module Hotep (

    self
  , spawn
  , spawnLink
  , spawnMonitor
  , exit
  , kill
  , killUnconditionally
  , send
  , link
  , unlink
  , monitor
  , unmonitor
  , trapExitsWith
  , passExits

) where

import Data.Void
import Data.Time.Clock (DiffTime)
import Hotep.Internal.Types
import Hotep.Internal.Config

-- | Get the @Config@ of the current process.
self :: Script m x (Config m x)
self = undefined

-- | Start a new process according to a script. A @Pid@ is returned so that you
-- can communicate with it.
spawn :: Script m x r -> Script m' x' (Pid m x)
spawn = undefined

-- | Spawn a process and immediately link it to the current one. This is similar
-- to calling @spawn@ and @link@ sequentially, but atomic.
spawnLink :: Script m x r -> Script m' x (Pid m x)
spawnLink = undefined

-- | Spawn a process and immediately create a monitor for it. This is similar to
-- calling @spawn@ and @monitor@ sequentially, but atomic.
spawnMonitor :: ((MonitorRef, Exit x) -> m') -> Script m x r -> Script m' x' (Pid m x, MonitorRef)
spawnMonitor = undefined

-- | Bidirectionally links the current process with a target process.
-- Terminations in either process forward their exit reason to the other. Links
-- persist until one of the processes is terminated or @unlink@ is called.
-- Idempotent.
link :: Pid m x -> Script m' x ()
link = undefined

-- | Break a bidirectional link between the current process and the target
-- process if one exists. Idempotent.
unlink :: Pid m x -> Script m' x' ()
unlink = undefined

-- | Creates a one-time-use monitor on a target process. Exit signals arising
-- from that process will be mapped to input messages on the current process.
-- Any number of monitors may be made and monitors behave unidirectionally.
monitor :: ((MonitorRef, Exit x) -> m') -> Pid m x -> Script m' x' MonitorRef
monitor = undefined

-- | Removes a monitor and guarantees that no monitoring messages will be
-- handled by the current process from that @MonitorRef@ (e.g., clears the
-- mailbox). (Note, in Erlang this is called "demonitor", but we use @unmonitor@
-- for parallelism with @unlink@).
unmonitor :: MonitorRef -> Script m x ()
unmonitor = undefined

-- | Send a message to a target process after a given interval.
sendAfter :: DiffTime -> m -> Pid m x -> Script m' x' ()
sendAfter = undefined

-- | Send a message to a target process immediately.
send :: m -> Pid m x -> Script m' x' ()
send = sendAfter 0

-- | Blocks the current process waiting on a message which passes the predicate.
-- Messages which don't pass the predicate remain in the mailbox in order. If a
-- timeout is given then after blocking so many microseconds the timeout
-- @Script@ is run instead.
selectiveReceiveOrTimeout :: Maybe (Int, Script m x r) -> (m -> Bool) -> (m -> Script m x r) -> Script m x r
selectiveReceiveOrTimeout = undefined

-- | Immediately terminate the calling process with reason @ex@.
exit :: x -> Script m x r
exit = undefined

-- | Send an exit signal to a target process with reason @ex@. The target
-- process may be trapping exits in which case it will interpret the kill reason
-- and determine whether to respond.
kill :: x -> Pid m x -> Script m' x ()
kill = undefined

-- | Similar to @kill@ but the target process cannot trap the exit.
killUnconditionally :: Pid m x -> Script m' x' ()
killUnconditionally = undefined

-- | Change behavior of current script to trap exit signals (and their
-- originating @Pid@s) and interpret them as messages with the passed handler.
trapExitsWith :: ((Exit x, SomePid) -> m) -> Script m x ()
trapExitsWith = undefined

-- | Configure behavior of current script to let exit signals kill the process.
-- Idempotent.
passExits :: Script m x ()
passExits = undefined
