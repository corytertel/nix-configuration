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
import Data.Ratio ((%)) -- for video
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position(End), Position(Above))
--import XMonad.Hooks.ManageDocks (avoidStruts, Direction2D (D, R, U))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
  (isFullscreen, isDialog,  doFullFloat, doCenterFloat, doRectFloat, composeOne, isInProperty)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.MultiToggle (mkToggle, single, Toggle (..))
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.TwoPane
import XMonad.Layout.ResizableTile
import XMonad.Layout.BinarySpacePartition
import XMonad.Util.NamedScratchpad
--import XMonad.Actions.Navigation2D
import XMonad.Actions.Navigation2D (switchLayer)
--import XMonad.Layout.WindowNavigation (windowNavigation, Direction2D(L, R, D, U), Navigate (Go, Swap))
import qualified XMonad.Layout.WindowNavigation as WN
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
myBorderWidth   = 8

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
--myWorkspaces    = [" \61728  ", " \62057  ", " \62074  ", " \61729  ", " \61564  ", " \61878  ", " \61441  ", " \61704  ", " \61612  "]
myWorkspaces    =   [ "1", "2", "3", "4", "5", "6", "7", "8", "9"]

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

    -- launch emacs
    , ((modm .|. shiftMask, xK_Return), spawn "emacsclient -c")

    -- launch rofi
    , ((modm,               xK_space     ), spawn "rofi -show run")

    -- launch dmenu
    --, ((modm .|. shiftMask, xK_space     ), spawn "dmenu_run")

    -- close focused window
    , ((modm,               xK_q     ), kill)

     -- Rotate through the available layout algorithms
    --, ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm,               xK_backslash ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_backslash ), sendMessage FirstLayout)

    --  Reset the layouts on the current workspace to default
    --, ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)


    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

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
    , ((0                 , xK_Alt_R), spawn "/home/cory/.nix-configuration/shared/apps/layout_switch/layout_switch.sh")

    -- Kill App
    , ((modm              , xK_Escape), spawn "xkill")

    -- Lock Screen
    , ((modm .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")

    -- Screenshot
    , ((modm .|. shiftMask, xK_s), spawn "flameshot full -p ~/Pictures")
    , ((modm              , xK_s), spawn "flameshot gui")

    -- Fullscreen
    , ((modm              , xK_f), sendMessage (Toggle NBFULL))

    -- Scratchpads
    , ((modm              , xK_apostrophe), namedScratchpadAction myScratchpads "terminal")
    , ((0                 , xF86XK_AudioPlay), namedScratchpadAction myScratchpads "cmus")

    -- Master and Stack Controls
    -- Resize viewed windows to the correct size
    --, ((modm,               xK_r     ), refresh)
    -- Move focus to the master window
    --, ((modm,               xK_m     ), windows W.focusMaster  )
    -- Move focus to the next window
    --, ((modm,               xK_Tab   ), windows W.focusDown)
    -- Move focus to the next window
    --, ((modm,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    --, ((modm,               xK_k     ), windows W.focusUp  )
    -- Swap the focused window and the master window
    --, ((modm,               xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    --, ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    --, ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Shrink the master area
    --, ((modm,               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    --, ((modm,               xK_l     ), sendMessage Expand)
    -- Resize resizableTall
    --, ((modm              , xK_m), sendMessage MirrorExpand)
    --, ((modm              , xK_n), sendMessage MirrorShrink)
    -- Increment the number of windows in the master area
    --, ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    --, ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Binary Space Partition Controls
    , ((modm .|. mod1Mask,                    xK_l     ), sendMessage $ ExpandTowards WN.R)
    , ((modm .|. mod1Mask,                    xK_h     ), sendMessage $ ExpandTowards WN.L)
    , ((modm .|. mod1Mask,                    xK_j     ), sendMessage $ ExpandTowards WN.D)
    , ((modm .|. mod1Mask,                    xK_k     ), sendMessage $ ExpandTowards WN.U)
    , ((modm .|. mod1Mask .|. controlMask ,   xK_l     ), sendMessage $ ShrinkFrom WN.R)
    , ((modm .|. mod1Mask .|. controlMask ,   xK_h     ), sendMessage $ ShrinkFrom WN.L)
    , ((modm .|. mod1Mask .|. controlMask ,   xK_j     ), sendMessage $ ShrinkFrom WN.D)
    , ((modm .|. mod1Mask .|. controlMask ,   xK_k     ), sendMessage $ ShrinkFrom WN.U)
    --, ((modm,                                xK_r     ), sendMessage Rotate)
    --, ((modm,                                xK_s     ), sendMessage Swap)
    --, ((modm,                                xK_n     ), sendMessage FocusParent)
    --, ((modm .|. controlMask,                xK_n     ), sendMessage SelectNode)
    --, ((modm .|. shiftMask,                  xK_n     ), sendMessage MoveNode)
    , ((modm .|. shiftMask .|. controlMask , xK_j     ), sendMessage $ SplitShift Prev)
    , ((modm .|. shiftMask .|. controlMask , xK_k     ), sendMessage $ SplitShift Next)
    , ((modm,               xK_a),     sendMessage Balance)
    , ((modm .|. shiftMask, xK_a),     sendMessage Equalize)

      -- Switch between layers
    , ((modm .|. shiftMask, xK_space), switchLayer)

    {-
    -- Directional navigation of windows
    , ((modm,                 xK_Right), windowGo R False)
    , ((modm,                 xK_Left ), windowGo L False)
    , ((modm,                 xK_Up   ), windowGo U False)
    , ((modm,                 xK_Down ), windowGo D False)

    , ((modm,                 xK_l), windowGo R False)
    , ((modm,                 xK_h), windowGo L False)
    , ((modm,                 xK_k), windowGo U False)
    , ((modm,                 xK_j), windowGo D False)
-}
    , ((modm, xK_h), sendMessage $ WN.Go WN.L)
    , ((modm, xK_j), sendMessage $ WN.Go WN.D)
    , ((modm, xK_k), sendMessage $ WN.Go WN.U)
    , ((modm, xK_l), sendMessage $ WN.Go WN.R)
    , ((modm, xK_n), windows W.focusUp)
    , ((modm, xK_n), windows W.focusDown)


    -- Swap adjacent windows
    , ((modm .|. controlMask, xK_l), sendMessage $ WN.Swap WN.R)
    , ((modm .|. controlMask, xK_h), sendMessage $ WN.Swap WN.L)
    , ((modm .|. controlMask, xK_k), sendMessage $ WN.Swap WN.U)
    , ((modm .|. controlMask, xK_j), sendMessage $ WN.Swap WN.D)

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
--myLayout = fullScreenToggle $ smartBorders $
--           (bigGaps   $ avoidStruts (binarySpacePartition))
--       ||| (smallGaps $ avoidStruts (resizableTile))
--       ||| (smallGaps $ avoidStruts (twoPane))
--       ||| (paperGaps $ avoidStruts (Full))
--       ||| (musicGaps $ avoidStruts (Mirror resizableTile))
--  where
myLayout = avoidStruts
         . WN.windowNavigation
         . smartBorders
         . fullScreenToggle $
           (bigGaps   $ binarySpacePartition)
       ||| (smallGaps $ binarySpacePartition)
       ||| (smallGaps $ twoPane)
       ||| (paperGaps $ Full)
       ||| (musicGaps $ Mirror binarySpacePartition)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     threeColumn = ThreeCol nmaster delta ratio
     threeColumnMid = ThreeColMid nmaster delta ratio
     twoPane = TwoPane (3/100) (1/2)
     resizableTile = ResizableTall 1 (3/100) (1/2) []
     binarySpacePartition = emptyBSP
     --dwindle = Dwindle 1 (3/100) (1/2)

     -- The defaulWN.t number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

     -- Fullscreen
     fullScreenToggle = mkToggle (single NBFULL)

     -- Spacing
     bigGaps   = spacingRaw False (Border 100 74 230 204)    True (Border 0 26 0 26) True
     smallGaps = spacingRaw False (Border 26 0 26 0)         True (Border 0 26 0 26) True
     paperGaps = spacingRaw False (Border 150 124 1050 1024) True (Border 0 26 0 26) True
     musicGaps = spacingRaw False (Border 300 274 860 834)   True (Border 0 26 0 26) True

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
    --[ insertPosition End Newer -- open new windows at the end
    [ insertPosition Above Newer -- open new windows above current window
    , className =? "MPlayer"                                          --> myRectFloat
    , className =? "mpv"                                              --> myRectFloat
    , className =? "vlc"                                              --> myRectFloat
    , className =? "Io.github.celluloid_player.Celluloid"             --> myRectFloat
    , className =? "gwenview"                                         --> myRectFloat
    , className =? "Firefox" <&&> resource =? "Toolkit"               --> myRectFloat
    , className =? "chromium-browser" <&&> isDialog                   --> myRectFloat
    , className =? "gimp"                                             --> doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog"       --> myRectFloat
    , stringProperty "WM_WINDOW_ROLE" =? "pop-up"                     --> myRectFloat
    , isDialog                                                        --> myRectFloat
    , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH" --> myRectFloat
    , title     =? "Save Image"                                       --> myRectFloat
    , title     =? "Save File"                                        --> myRectFloat
    , title     =? "Open"                                             --> myRectFloat
    , title     =? "Open Files"                                       --> myRectFloat
    , resource  =? "desktop_window"                                   --> doIgnore
    , resource  =? "kdesktop"                                         --> doIgnore
    , isFullscreen                                                    --> doFullFloat
    ] <+> namedScratchpadManageHook myScratchpads
  where
    unfloat = ask >>= doF . W.sink
    myRectFloat = doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = fullscreenEventHook

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
myBar0 = "xmobar $HOME/.config/xmobar/xmobarrc0"
myBar1 = "xmobar $HOME/.config/xmobar/xmobarrc1"
myBar2 = "xmobar $HOME/.config/xmobar/xmobarrc2"

myPP = xmobarPP { ppCurrent = xmobarColor "#f0f0f0" "" . wrap "[" "]" --current selected desktop
                , ppHidden = xmobarColor "#f0f0f0" "" . wrap "" ""
                , ppHiddenNoWindows = xmobarColor "#707070" "" . wrap "" "" --desktops with no windows
                , ppVisible = xmobarColor "#f0f0f0" "" . wrap "" ""
                , ppTitle = xmobarColor "#f0f0f0" "" . shorten 40
                , ppOrder = \(ws:_:_:_) -> [ws]
                }

myPP2 = xmobarPP { ppOrder = \(_:_:_:_) -> [] }

-- Key binding to toggle the gap from the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

------------------------------------------------------------------------
-- Scratchpads
myScratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "cmus" spawnCmus findCmus manageCmus
                ]
  where
    spawnTerm  = myTerminal ++ " --name=scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)
    spawnCmus  = myTerminal ++ " --name=cmus 'cmus'"
    findCmus   = resource =? "cmus"
    manageCmus = customFloating $ W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
--main = xmonad . ewmh . withNavigation2DConfig def =<< statusBar myBar0 myPP toggleStrutsKey =<< statusBar myBar1 myPP2 toggleStrutsKey =<< statusBar myBar2 myPP2 toggleStrutsKey defaults
main = xmonad . ewmh =<< statusBar myBar0 myPP toggleStrutsKey =<< statusBar myBar1 myPP2 toggleStrutsKey =<< statusBar myBar2 myPP2 toggleStrutsKey defaults

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
