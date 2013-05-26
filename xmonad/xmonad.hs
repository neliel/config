import XMonad
import XMonad.Config.Azerty
import Graphics.X11.ExtraTypes.XF86
import qualified Data.Map as M
import XMonad.Hooks.DynamicLog
--
import XMonad.Actions.Volume
import XMonad.Util.Dzen
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

-- Main configuration, override the defaults to your liking.
myConf = defaultConfig
                      { modMask     = mod4Mask
                      , terminal    = "urxvt"
                      , layoutHook  = smartBorders $ myLayout
                      , workspaces  = myWorkspaces
                      , manageHook  = myManageHook <+> manageHook defaultConfig
                      , startupHook = myStartupHook
                      , keys        = myKeys
                      }

-- | Workspaces redirection
myManageHook = composeAll
    [ className =? "Emacs"     --> doShift "2:edit"
    , className =? "Firefox"   --> doShift "3:web"
    , className =? "Thunar"    --> doShift "4:browse"
    , className =? "Pidgin"    --> doShift "6:pidgin"
    , className =? "Smplayer"  --> doFloat
    , appName   =? "Irssi"     --> doShift "5:irc"
    ]

myStartupHook = do
  spawn "feh --bg-scale /home/sarfraz/Images/1367684534_4464.jpg &"
  spawn "pidgin"
  spawn "emacs"
  spawn "thunar"
  spawn "urxvt -name Irssi -e screen irssi"
  spawn "screen -d -m rtorrent"

alert    = dzenConfig centered . show . round
centered =
        onCurr (center 150 66)
    >=> font "-*-Inconsolata-*-12-*-*-*-*-*-*-*"
    >=> addArgs ["-bg", "black"]
    >=> addArgs ["-fg", "grey"]

-- | Keyboard keys
keysToAdd x =
    [ ((mod4Mask, xK_F4                   ), kill)
    , ((0, xF86XK_Calculator              ), spawn "gnome-calculator")
    , ((0, xF86XK_AudioLowerVolume        ), lowerVolume 10 >>= alert)
    , ((0, xF86XK_AudioRaiseVolume        ), raiseVolume 10 >>= alert)
    , ((0, xF86XK_WWW                     ), spawn "firefox")
    , ((0, xF86XK_HomePage                ), spawn "thunar")
    , ((0, xF86XK_Search                  ), spawn "pidgin")
    , ((0, xK_Print                       ), spawn "gnome-screenshot")
    , ((mod4Mask, xK_f                    ), spawn "firefox")
    , ((mod4Mask, xK_s                    ), spawn "thunar")
    , ((mod4Mask, xK_z                    ), spawn "emacs")
    ]

keysToDel x = [((mod4Mask .|. shiftMask), xK_c)] -- to delete the unused keys

myKeys x = foldr M.delete (keysToAdd' x) (keysToDel x)
  where
    -- to include new keys to existing keys
    keysToAdd' x = M.union (keys azertyConfig x) (M.fromList (keysToAdd x))

-- | Workspaces listing
myWorkspaces = ["1:main", "2:edit", "3:web", "4:browse", "5:irc", "6:pidgin"]

-- | Default layout
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
