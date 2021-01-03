import XMonad
import XMonad.Hooks.DynamicLog
-- import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO
import System.Exit (exitSuccess)

-- import XMonad.Layout.Spiral
import XMonad.Layout.BinarySpacePartition
-- import XMonad.Layout.Tabbed

-- import XMonad.Layout.Dwindle

import qualified XMonad.StackSet as W

myKeys :: [ ([Char], X ())]
myKeys =
    [ ("M-q", kill)
    , ("M-S-r", spawn "xmonad --restart")
    , ("M-S-q", io exitSuccess)
    ]

myLayoutHook = avoidStruts $ emptyBSP ||| layoutHook defaultConfig

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

main :: IO()
main = do
        xmproc <- spawnPipe "xmobar /home/tom/.config/xmobarrc0"
        _ <- spawnPipe "/home/tom/.config/startup.sh"
        xmonad $ docks defaultConfig {
        -- layoutHook  =  avoidStruts  $  layoutHook  defaultConfig
        layoutHook = myLayoutHook
                ,  logHook  =  dynamicLogWithPP  xmobarPP
                        { ppOutput  =  hPutStrLn  xmproc
                        , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#98be65" ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#c792ea" ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
                        , ppSep =  "<fc=#666666> <fn=2>|</fn> </fc>"          -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
                ,  modMask = mod1Mask
                ,  terminal = "st"
        } `additionalKeysP` myKeys

