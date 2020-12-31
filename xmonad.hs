import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO


main = do
        xmproc <- spawnPipe "xmobar /home/tom/.config/xmobarrc"
        xmonad defaultConfig {
        layoutHook  =  avoidStruts  $  layoutHook  defaultConfig
                ,  logHook  =  dynamicLogWithPP  xmobarPP
                        {  ppOutput  =  hPutStrLn  xmproc
                        ,  ppTitle  =  xmobarColor  "green"  ""  .  shorten  50
                        }
                ,  modMask = mod4mask
                ,  terminal = "st"
                }
