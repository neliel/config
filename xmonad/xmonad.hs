{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

import XMonad
-- keyboard
import XMonad.Actions.CycleWS
import Graphics.X11.ExtraTypes.XF86
import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
--
import XMonad.Layout.NoBorders
--
import Data.Ratio
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.IM

main :: IO ()
main = xmonad =<< xmobar myConf

-- |Main configuration, override the defaults to your liking.
myConf = defaultConfig
   { modMask     = mod4Mask
   , terminal    = "urxvt"
   , layoutHook  = smartBorders $ myLayout
   , workspaces  = myWS
   , manageHook  = myManageHook <+> manageHook defaultConfig
   , startupHook = myStartupHook
   , keys        = myKeys }

-- |Workspaces redirection
myManageHook = composeAll
    [ className =? "Firefox"   --> doShift "3:web"
    , className =? "Pidgin"    --> doShift "6:pidgin"
    , className =? "Caja"      --> doShift "4:browse"
    , appName   =? "Irssi"     --> doShift "5:irc" ]

-- |Programs to start at login
myStartupHook = do
   spawn "pidgin"
   spawn "caja"
   spawn "urxvt -name Irssi -e screen irssi"

-- |Keyboard keys

homeMask :: KeyMask
homeMask =  133 -- from the xev data

keysToAdd x =
    [ ((mod4Mask, xK_F4                   ), kill)
    , ((0, xF86XK_Forward                 ), nextWS)
    , ((0, xF86XK_Back                    ), prevWS)
    , ((0, xK_Print                       ), spawn "mate-screenshot")
    , ((0, xF86XK_Calculator              ), spawn "mate-calculator")
    , ((0, xF86XK_WWW                     ), spawn "firefox")
    , ((0, xF86XK_HomePage                ), spawn "caja") ]
    ++
    [((m .|. homeMask, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces defaultConfig) [10 .. 15]
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] ]

keysToDel x = [((mod4Mask .|. shiftMask), xK_c)] -- to delete the unused keys

myKeys x = foldr M.delete (keysToAdd' x) (keysToDel x)
  where
    -- to include new keys to existing keys
    keysToAdd' x = M.union (keys defaultConfig x) (M.fromList (keysToAdd x))

-- |Workspaces listing
myWS = ["1:main", "2:edit", "3:web", "4:browse", "5:irc", "6:pidgin"]

-- |Default layout
myLayout = pidgin $ Mirror tiled ||| tiled ||| Full
    where
        -- pidgin conf
        pidgin       = onWorkspace "6:pidgin" pidginLayout
        pidginLayout = withIM (18/100) (Role "buddy_list") gridLayout
        gridLayout   = spacing 8 $ Grid

        -- default tiling algorithm partitions the screen into two panes
        tiled = spacing 2 $ Tall nmaster delta ratio
        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio = 2/3
        -- Percent of screen to increment by when resizing panes
        delta = 5/100
--eof
