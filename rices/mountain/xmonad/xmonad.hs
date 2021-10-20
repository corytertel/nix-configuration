--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
-- Normally, you'd only override those defaults you care about.
--
import XMonad
import Data.Monoid (mappend)
import Data.Map (fromList)
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position(End))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
--import XMonad.Actions.Volume

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "kitty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 4

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
--myWorkspaces    = ["\61728 ", "\62057 ", "\62074 ", "\61729 ", "\61564 ", "\61878 ", "\61441 ", "\61704 ", "\61612 "]
myWorkspaces    = ["\61728 ", "\62056 ", "\62074 ", "\61729 ", "\61564 ", "\61878 ", "\61441 ", "\61704 ", "\61612 "]
{-
myWorkspaces    =   [ "<icon=/home/cory/.nix-configuration/pc/system/xmonad/icons/kitty_logo.xpm/>"
                    , "<icon=/home/cory/.nix-configuration/pc/system/xmonad/icons/chromium_logo.xpm/>"
                    , "<icon=/home/cory/.nix-configuration/pc/system/xmonad/icons/discord_logo.xpm/>"
                    , "<icon=/home/cory/.nix-configuration/pc/system/xmonad/icons/spacemacs_logo.xpm/>"
                    , "<icon=/home/cory/.nix-configuration/pc/system/xmonad/icons/dolphin_logo.xpm/>"
                    , "<icon=/home/cory/.nix-configuration/pc/system/xmonad/icons/steam_logo.xpm/>"
                    , "\61441 "
                    , "\61704 "
                    , "\61612 "]
--}

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#0f0f0f"
myFocusedBorderColor = "#f0f0f0"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    --[ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)

    -- launch rofi
    , ((modm,               xK_space     ), spawn "rofi -show run")

    -- launch dmenu
    , ((modm .|. shiftMask, xK_space     ), spawn "dmenu_run")

    -- close focused window
    , ((modm,               xK_q     ), kill)

     -- Rotate through the available layout algorithms
    --, ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm,               xK_backslash ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    --, ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm .|. shiftMask, xK_r     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))

    -- Volume
    --, ((0                 , xF86XK_AudioLowerVolume), lowerVolume 4 >> return())
    --, ((0                 , xF86XK_AudioRaiseVolume), raiseVolume 4 >> return())
    --, ((0                 , xF86XK_AudioMute), toggleMute >> return())
    , ((0                 , xF86XK_AudioLowerVolume), spawn "pamixer --decrease 2")
    , ((0                 , xF86XK_AudioRaiseVolume), spawn "pamixer --increase 2")
    , ((0                 , xF86XK_AudioMute), spawn "pamixer --toggle-mute")

    -- Brightness
    , ((0                 , xF86XK_MonBrightnessUp), spawn "xbrightness +5000")
    , ((0                 , xF86XK_MonBrightnessDown), spawn "xbrightness -5000")

    -- Keyboard Layout
    --, ((0                 , xK_Alt_R), spawn "/home/cory/.nix-configuration/shared/users/cory/apps/layout_switch/layout_switch.sh")

    -- Kill App
    , ((modm              , xK_Escape), spawn "xkill")

    -- Lock Screen
    , ((modm .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")

    -- Screenshot
    , ((modm .|. shiftMask, xK_s), spawn "flameshot full -p ~/Pictures")
    , ((modm              , xK_s), spawn "flameshot gui")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
--myLayout = spacingRaw False (Border 50 0 50 0) True (Border 0 50 0 50) True $
--myLayout = spacingRaw False (Border 150 100  430 380) True (Border 0 50 0 50) True $
--        avoidStruts (tiled ||| Mirror tiled ||| threeColumn ||| threeColumnMid ||| Full)
myLayout = (spacingRaw False (Border 150 100 430 380) True (Border 0 50 0 50) True $ avoidStruts (tiled))
       ||| (spacingRaw False (Border 50 0 50 0) True (Border 0 50 0 50) True $ avoidStruts (tiled))
       ||| (spacingRaw False (Border 50 0 50 0) True (Border 0 50 0 50) True $ avoidStruts (threeColumnMid))
       |||  Full
       ||| (spacingRaw False (Border 150 100 430 380) True (Border 0 50 0 50) True $ avoidStruts (Mirror tiled))
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     threeColumn = ThreeCol nmaster delta ratio
     threeColumnMid = ThreeColMid nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ insertPosition End Newer -- open new windows at the end
    , className =? "MPlayer"        --> doFloat
    , className =? "vlc"           --> doFloat
    , className =? "mpv"           --> doFloat
    , className =? "io.github.celluloid_player.Celluloid" --> doFloat
    , className =? "gwenview"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , title     =? "Save Image"     --> unfloat
    , title     =? "Save File"     --> unfloat
    , title     =? "Open"     --> unfloat
    , title     =? "Open Files"     --> unfloat
    ] where unfloat = ask >>= doF . W.sink

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

  -- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
        spawnOnce "feh --bg-fill $HOME/Pictures/wallpaper.jpg"
        spawnOnce "picom &"

------------------------------------------------------------------------
-- Command to launch the bar.
myBar  = "xmobar $HOME/.config/xmobar/xmobarrc"
myBar0 = "xmobar $HOME/.config/xmobar/xmobarrc0"
myBar1 = "xmobar $HOME/.config/xmobar/xmobarrc1"
myBar2 = "xmobar $HOME/.config/xmobar/xmobarrc2"

myPP = xmobarPP { ppCurrent = xmobarColor "#ac8a8c" "" . wrap "[" "]" --current selected desktop
                , ppHidden = xmobarColor "#f0f0f0" "" . wrap "" ""
                , ppHiddenNoWindows = xmobarColor "#f0f0f0" "" . wrap "" "" --desktops with no windows
                , ppVisible = xmobarColor "#f0f0f0" "" . wrap "" ""
                , ppTitle = xmobarColor "#f0f0f0" "" . shorten 40
                , ppOrder = \(ws:_:_:_) -> [ws]
                }

myPP2 = xmobarPP { ppOrder = \(_:_:_:_) -> [] }

-- Key binding to toggle the gap from the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad =<< statusBar myBar0 myPP toggleStrutsKey =<< statusBar myBar1 myPP2 toggleStrutsKey =<< statusBar myBar2 myPP2 toggleStrutsKey defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
