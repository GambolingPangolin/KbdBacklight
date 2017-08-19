{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

module Main (main) where

import           Control.Concurrent    (threadDelay)
import           Control.Monad
import qualified Data.Map              as M
import           DBus
import           DBus.Client
import           DBus.Internal.Address (Address (..))

ledPath :: String
ledPath = "/sys/class/leds/asus::kbd_backlight/brightness"

report :: Int -> Int -> IO ()
report n n' = putStrLn $ "Changing brightness from " ++ show n ++ " to " ++ show n'

incKbdBacklight :: IO ()
incKbdBacklight = do
  n <- read <$> readFile ledPath :: IO Int
  let n' = min 3 (n+1)
  report n n'
  writeFile ledPath (show n')

matchInc :: MatchRule
matchInc = matchAny
  { matchInterface = Just "rotibula.KbdBacklight"
  , matchMember = Just "inc" }

decKbdBacklight :: IO ()
decKbdBacklight = do
  n <- read <$> readFile ledPath :: IO Int
  let n' = max 0 (n-1)
  report n n'
  writeFile ledPath (show n')

matchDec :: MatchRule
matchDec = matchAny
  { matchInterface = Just "rotibula.KbdBacklight"
  , matchMember = Just "dec" }

main :: IO ()
main = do
  c <- connectSystem
  requestName c "rotibula.KbdBacklight" []
  export c "/rotibula/kbd_backlight"
    [ autoMethod "rotibula.KbdBacklight" "inc" incKbdBacklight
    , autoMethod "rotibula.KbdBacklight" "dec" decKbdBacklight ]
  addMatch c matchInc (const incKbdBacklight)
  addMatch c matchDec (const decKbdBacklight)
  forever (threadDelay 50000)

