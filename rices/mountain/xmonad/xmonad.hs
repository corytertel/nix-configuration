import XMonad

import Data.Monoid (mappend)
import Data.Map (fromList)
import Data.Ratio ((%)) -- for video

import Graphics.X11.ExtraTypes.XF86

import System.Exit

import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig

import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.MultiToggle (mkToggle, single, Toggle (..))
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.TwoPane
import XMonad.Layout.ResizableTile
import XMonad.Layout.BinarySpacePartition

import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position(End), Position(Above))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageHelpers
  (isFullscreen, isDialog,  doFullFloat, doCenterFloat, doRectFloat, composeOne, isInProperty)

import XMonad.Actions.Navigation2D (switchLayer)
import XMonad.Actions.FloatKeys

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Layout.WindowNavigation as WN

myTerminal      = "kitty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 0

myModMask       = mod4Mask

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

myNormalBorderColor  = "#0f0f0f"
myFocusedBorderColor = "#f0f0f0"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myAdditionalKeys :: [(String, X ())]
myAdditionalKeys =
    -- launch a terminal
    [ ("M-<Return>", spawn $ myTerminal)
    -- launch emacs
    , ("M-S-<Return>", spawn "emacsclient -c")
    -- launch rofi
    , ("M-<Space>", spawn "rofi -show run")
    -- close focused window
    , ("M-q", kill)
     -- Rotate through the available layout algorithms
    , ("M-\\", sendMessage NextLayout)
    , ("M-S-\\", sendMessage FirstLayout)
    -- Push window back into tiling
    , ("M-t", withFocused $ windows . W.sink)
    -- Quit xmonad
    , ("M-S-q", io (exitWith ExitSuccess))
    -- Restart xmonad
    , ("M-C-r", spawn "xmonad --recompile; xmonad --restart")
    -- Volume
    , ("<XF86AudioLowerVolume>", spawn "pamixer --decrease 2")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer --increase 2")
    , ("<XF86AudioMute>", spawn "pamixer --toggle-mute")
    -- Brightness
    , ("<XF86MonBrightnessUp>", spawn "xbrightness +5000")
    , ("<XF86MonBrightnessDown>", spawn "xbrightness -5000")
    -- Keyboard Layout
    , ("M3-<Space>", spawn "/home/cory/.nix-configuration/shared/apps/layout_switch/layout_switch.sh")
    -- Kill App
    , ("M-<Escape>", spawn "xkill")
    -- Lock Screen
    , ("M-S-z", spawn "xscreensaver-command -lock")
    -- Screenshot
    , ("M-<Print>", spawn "flameshot full -p ~/Pictures")
    , ("M-S-<Print>", spawn "flameshot gui")
    -- Fullscreen
    , ("M-f", sendMessage (Toggle NBFULL))
    -- Scratchpads
    , ("M-'", namedScratchpadAction myScratchpads "terminal")
    , ("<XF86AudioPlay>", namedScratchpadAction myScratchpads "cmus")
    -- Master and Stack Controls
    -- Resize viewed windows to the correct size
    --, ("M-r", refresh)
    -- Move focus to the master window
    --, ("M-m", windows W.focusMaster  )
    -- Move focus to the next window
    --, ("M-<Tab>", windows W.focusDown)
    -- Move focus to the next window
    --, ("M-j", windows W.focusDown)
    -- Move focus to the previous window
    --, ("M-k", windows W.focusUp  )
    -- Swap the focused window and the master window
    --, ("M-<Return>", windows W.swapMaster)
    -- Swap the focused window with the next window
    --, ("M-S-j", windows W.swapDown  )
    -- Swap the focused window with the previous window
    --, ("M-S-k", windows W.swapUp    )
    -- Shrink the master area
    --, ("M-h", sendMessage Shrink)
    -- Expand the master area
    --, ("M-l", sendMessage Expand)
    -- Resize resizableTall
    --, ("M-m", sendMessage MirrorExpand)
    --, ("M-n", sendMessage MirrorShrink)
    -- Increment the number of windows in the master area
    --, ("M-,", sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    --, ("M-.", sendMessage (IncMasterN (-1)))
    -- Binary Space Partition Controls
    , ("M-M1-l", sendMessage $ ExpandTowards WN.R)
    , ("M-M1-h", sendMessage $ ExpandTowards WN.L)
    , ("M-M1-j", sendMessage $ ExpandTowards WN.D)
    , ("M-M1-k", sendMessage $ ExpandTowards WN.U)
    , ("M-M1-C-l", sendMessage $ ShrinkFrom WN.R)
    , ("M-M1-C-h", sendMessage $ ShrinkFrom WN.L)
    , ("M-M1-C-j", sendMessage $ ShrinkFrom WN.D)
    , ("M-M1-C-k", sendMessage $ ShrinkFrom WN.U)
    --, ("M-r", sendMessage RotateL)
    --, ("M-S-r", sendMessage RotateR)
    , ("M-r", sendMessage Rotate)
    , ("M-s", sendMessage Swap)
    --, ("M-n", sendMessage FocusParent)
    --, ("M-C-n", sendMessage SelectNode)
    --, ("M-S-n", sendMessage MoveNode)
    , ("M-S-C-j", sendMessage $ SplitShift Prev)
    , ("M-S-C-k", sendMessage $ SplitShift Next)
    , ("M-v",     sendMessage Balance)
    , ("M-S-v",     sendMessage Equalize)
    -- Switch between layers
    , ("M-S-<Space>", switchLayer)
    -- Directional navigation of windows
    , ("M-h", sendMessage $ WN.Go WN.L)
    , ("M-j", sendMessage $ WN.Go WN.D)
    , ("M-k", sendMessage $ WN.Go WN.U)
    , ("M-l", sendMessage $ WN.Go WN.R)
    , ("M-m", windows W.focusUp)
    , ("M-n", windows W.focusDown)
    -- Swap adjacent windows
    , ("M-C-l", sendMessage $ WN.Swap WN.R)
    , ("M-C-h", sendMessage $ WN.Swap WN.L)
    , ("M-C-k", sendMessage $ WN.Swap WN.U)
    , ("M-C-j", sendMessage $ WN.Swap WN.D)
    -- Float keys
    , ("M-<U>", withFocused (keysMoveWindow (0,-40)))
    , ("M-<D>", withFocused (keysMoveWindow (0, 40)))
    , ("M-<L>", withFocused (keysMoveWindow (-40,0)))
    , ("M-<R>", withFocused (keysMoveWindow (40, 0)))
    , ("M-<U>-<L>", withFocused (keysMoveWindow (-40,-40)))
    , ("M-<U>-<R>", withFocused (keysMoveWindow ( 40,-40)))
    , ("M-<D>-<L>", withFocused (keysMoveWindow (-40, 40)))
    , ("M-<D>-<R>", withFocused (keysMoveWindow ( 40, 40)))
    , ("M-M1-<U>", withFocused (keysResizeWindow (0,40) (1,0)))
    , ("M-M1-<D>", withFocused (keysResizeWindow (0,40) (0,0)))
    , ("M-M1-<L>", withFocused (keysResizeWindow (40,0) (1,0)))
    , ("M-M1-<R>", withFocused (keysResizeWindow (40,0) (0,0)))
    , ("M-M1-C-<U>", withFocused (keysResizeWindow (0,-40) (1,0)))
    , ("M-M1-C-<D>", withFocused (keysResizeWindow (0,-40) (0,0)))
    , ("M-M1-C-<L>", withFocused (keysResizeWindow (-40,0) (1,0)))
    , ("M-M1-C-<R>", withFocused (keysResizeWindow (-40,0) (0,0)))
    , ("M-S-<L>", withFocused (keysAbsResizeWindow (-40,-40) (2048,1504)))
    , ("M-S-<R>", withFocused (keysAbsResizeWindow ( 40, 40) (2048,1504)))
    ]

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
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
     -- Fullscreen
     fullScreenToggle = mkToggle (single NBFULL)
     -- Spacing
     bigGaps   = spacingRaw False (Border 150 135 430 415)   True (Border 0 15 0 15) True
     smallGaps = spacingRaw False (Border 50 35 50 35)       True (Border 0 15 0 15) True
     paperGaps = spacingRaw False (Border 150 150 1250 1250) True (Border 0  0 0  0) True
     musicGaps = spacingRaw False (Border 300 285 860 845)   True (Border 0 15 0 15) True

------------------------------------------------------------------------

myManageHook = composeAll
    [ insertPosition Above Newer -- open new windows above current window
    , className =? "MPlayer"                                          --> myRectFloat
    , className =? "mpv"                                              --> myRectFloat
    , className =? "vlc"                                              --> myRectFloat
    , className =? "Io.github.celluloid_player.Celluloid"             --> myRectFloat
    , className =? "gwenview"                                         --> myRectFloat
    , className =? "Firefox" <&&> resource =? "Toolkit"               --> myRectFloat
    , className =? "chromium-browser" <&&> isDialog                   --> myRectFloat
    , className =? "Gimp"                                             --> doFloat
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

myEventHook = fullscreenEventHook

------------------------------------------------------------------------

myLogHook = return ()

------------------------------------------------------------------------

myStartupHook = do
        spawnOnce "feh --bg-fill $HOME/Pictures/wallpaper.jpg"
        spawnOnce "picom &"

------------------------------------------------------------------------

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

myScratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "cmus" spawnCmus findCmus manageCmus
                ]
  where
    spawnTerm   = myTerminal ++ " --name=scratchpad"
    findTerm    = resource =? "scratchpad"
    manageTerm  = customFloating $ W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)
    spawnCmus   = myTerminal ++ " --name=cmus 'cmus'"
    findCmus    = resource =? "cmus"
    manageCmus  = customFloating $ W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)

------------------------------------------------------------------------

main = xmonad . ewmh =<< statusBar myBar0 myPP toggleStrutsKey =<< statusBar myBar1 myPP2 toggleStrutsKey =<< statusBar myBar2 myPP2 toggleStrutsKey defaults

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
    } `additionalKeysP` myAdditionalKeys
