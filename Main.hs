{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           DBus
import           DBus.Client
import qualified System.IO.Strict   as SIO

-- This is the path to the file that contains the current brightness setting.
-- The file `max_brightness` in the same directory contains the value 3, so we
-- infer that the possible brightness values are 0, 1, 2, and 3.
ledPath :: String
ledPath = "/sys/class/leds/asus::kbd_backlight/brightness"

-- Compute the current brightness setting by reading the value from the
-- brightness file
bValue :: IO Int
bValue = read <$> SIO.readFile ledPath

-- Increment the value in the brightness file up to a maximum brightness of 3
incKbdBacklight :: IO ()
incKbdBacklight = do
  n <- bValue
  let n' = min 3 (n+1)
  writeFile ledPath $ show n'

-- Match rule for `localhost.KbdBacklight.inc`
matchInc :: MatchRule
matchInc = matchAny
  { matchInterface = Just "localhost.KbdBacklight"
  , matchMember = Just "inc" }

-- Decrement the value in the brightness file down to a minimum of 0
decKbdBacklight :: IO ()
decKbdBacklight = do
  n <- bValue
  let n' = max 0 (n-1)
  writeFile ledPath $ show n'

-- Match rule for `localhost.KbdBacklight.dec`
matchDec :: MatchRule
matchDec = matchAny
  { matchInterface = Just "localhost.KbdBacklight"
  , matchMember = Just "dec" }

main :: IO ()
main = do
  -- Create a client connection to the system bus
  c <- connectSystem

  -- Register our name
  requestName c "localhost.KbdBacklight" []

  -- Export `incKbdBacklight` as `inc` and `decKbdBacklight` as `dec`
  export c "/localhost/KbdBacklight"
    [ autoMethod "localhost.KbdBacklight" "inc" incKbdBacklight
    , autoMethod "localhost.KbdBacklight" "dec" decKbdBacklight ]

  -- Listeners for the methods `inc` and `dec`
  addMatch c matchInc $ const incKbdBacklight
  addMatch c matchDec $ const decKbdBacklight

  -- Wait around for signals from DBus
  forever $ threadDelay 50000
