import XMonad
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import Graphics.X11.ExtraTypes.XF86
import qualified Data.Map as M
--
import Data.Ratio
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.IM


-- The main function.
main = xmonad =<< xmobar myConf

-- Main configuration, override the defaults to your liking.
myConf = defaultConfig
    { modMask    = mod4Mask
    , terminal   = "urxvtc"
    , layoutHook = myLayout
    , workspaces = myWorkspaces
    , manageHook = myManageHook <+> manageHook defaultConfig
    , keys       = myKeys
    }

-- | Workspaces redirection
myManageHook = composeAll
    [ className =? "Chromium" --> doShift "3:web"
    , className =? "Spacefm"  --> doShift "4:browse"
    , className =? "Pidgin"   --> doShift "5:connect"
    ]

-- | Keyboard keys
keysToAdd x =
    [ ((mod4Mask, xK_F4          ), kill            )
    , ((mod4Mask, xF86XK_WWW     ), spawn "chromium")
    , ((mod4Mask, xF86XK_HomePage), spawn "spacefm" )
    , ((mod4Mask, xF86XK_Search  ), spawn "pidgin"  )
    ]

keysToDel x = [((mod4Mask .|. shiftMask), xK_c)] -- to delete the unused keys

myKeys x = foldr M.delete (keysToAdd' x) (keysToDel x)
  where
    -- to include new keys to existing keys
    keysToAdd' x = M.union (keys azertyConfig x) (M.fromList (keysToAdd x))

-- | Workspaces listing
myWorkspaces = ["1:main", "2:edit", "3:web", "4:browse", "5:connect", "6:tmp"]

-- | Default layout
myLayout = pidgin $ Mirror tiled ||| tiled ||| Full
    where
        -- pidgin conf
        pidgin       = onWorkspace "5:connect" pidginLayout
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
--EOF
