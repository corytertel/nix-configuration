{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wunused-imports #-}

import XMonad hiding ((|||))

import Data.Monoid (mappend)
import Data.Map (fromList, lookup)
import Data.Maybe (fromJust)
import Data.Ratio ((%)) -- for video

import Graphics.X11.ExtraTypes.XF86

import System.Exit

import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.Image

import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.MultiToggle (mkToggle, single, Toggle (..))
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.IfMax
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Tabbed
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.ComboP
import XMonad.Layout.TwoPane
import XMonad.Layout.BinarySpacePartition

import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageHelpers
  (isFullscreen, isDialog,  doFullFloat, doCenterFloat, doRectFloat, composeOne, isInProperty)

import XMonad.Actions.Navigation2D (switchLayer)
import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap
import XMonad.Actions.Search
import XMonad.Actions.WindowMenu
import XMonad.Actions.Minimize
import XMonad.Actions.TagWindows
import XMonad.Actions.CycleWS (nextWS, prevWS)
--import XMonad.Actions.TiledWindowDragging
--import XMonad.Layout.DraggingVisualizer

import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Layout.WindowNavigation as WN
import qualified XMonad.Layout.BoringWindows as BW

myTerminal      = "urxvtc -icon $HOME/.icons/icons/48x48/terminal.png"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 1

myNormalBorderColor  = "#1e2731"
myFocusedBorderColor = "#81a1c1"

myModMask       = mod4Mask

ws1 = " \61728 "
ws2 = " \62057 "
ws3 = " \62074 "
ws4 = " \61729 "
ws5 = " \61564 "
ws6 = " \61878 "
ws7 = " \61441 "
ws8 = " \61704 "
ws9 = " \61612 "

myWorkspaces    = [ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
  where i = fromJust $ M.lookup ws myWorkspaceIndices

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
    --[ ("M-<Return>", spawn $ myTerminal)
    -- launch emacs
    --, ("M-S-<Return>", spawn "emacsclient -c")
    -- Xmonad prompt
    [ ("M-x", shellPrompt launcherXPConfig)
    --, ("M-<Space>", spawn "rofi -matching normal -show drun -modi drun,run -show-icons")
    -- Xmonad prefix prompt
    , ("M-z", prefixPrompt)
    , ("M-<Space>", prefixPrompt)
    , ("M1-<Space>", prefixPrompt)
    -- Xmonad command prompt
    --, ("M-x", commandPrompt)
    -- Emacs launcher
    , ("M-c", spawn "emacsclient --eval '(emacs-run-launcher)'")
    -- File Manager
    --, ("M-e", spawn "pcmanfm --new-win")
    -- close focused window
    --, ("M-q", kill)
    -- Rotate through the available layout algorithms
    --, ("M-\\", sendMessage NextLayout)
    --, ("M-S-\\", sendMessage FirstLayout)
    -- Push window back into tiling
    , ("M-t", withFocused $ windows . W.sink)
    -- Quit xmonad
    --, ("M-S-q", io (exitWith ExitSuccess))
    -- Restart xmonad
    --, ("M-C-r", spawn "xmonad --recompile; xmonad --restart")
    -- Audio Controls
    , ("<XF86AudioLowerVolume>", spawn "pamixer --decrease 2")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer --increase 2")
    , ("<XF86AudioMute>", spawn "pamixer --toggle-mute")
    , ("<XF86AudioNext>", spawn "audacious --fwd")
    , ("<XF86AudioPrev>", spawn "audacious --rew")
    , ("<XF86AudioPlay>", spawn "audacious --play-pause")
    , ("<XF86AudioStop>", spawn "audacious --stop")
    , ("<F5>", spawn "pamixer --decrease 2")
    , ("<F6>", spawn "pamixer --increase 2")
    , ("<F7>", spawn "pamixer --toggle-mute")
    , ("<F10>", spawn "audacious --fwd")
    , ("<F9>", spawn "audacious --rew")
    , ("<F8>", spawn "audacious --play-pause")
    -- Brightness
    , ("<XF86MonBrightnessUp>", spawn "xbrightness +5000")
    , ("<XF86MonBrightnessDown>", spawn "xbrightness -5000")
    -- Keyboard Layout
    , ("M-C-<Space>", spawn "/home/cory/manual_installs/layout_switch.sh")
    -- Kill App
    , ("M-<Escape>", spawn "xkill")
    -- Lock Screen
    --, ("M-S-z", spawn "xscreensaver-command -lock")
    -- Screenshot
    --, ("M-<Print>", spawn "flameshot full -p ~/Screenshots/")
    --, ("M-S-<Print>", spawn "flameshot gui")
    -- Fullscreen
    , ("M-f", sendMessage (Toggle NBFULL))
    -- Minimize
    , ("M-i", withFocused minimizeWindow)
    , ("M-S-i", withLastMinimized maximizeWindowAndFocus)
    -- Maximize
    --, ("M-f", withFocused (sendMessage . maximizeRestore))
    -- Window Menu
    , ("M-o", windowMenu)
    -- Scratchpads
    --, ("M-'", namedScratchpadAction myScratchpads "terminal")
    --, ("M-0", namedScratchpadAction myScratchpads "audacious")
    --, ("M-'", spawn $ (myTerminal ++ " -name scratchpad"))
    --, ("M-0", spawn "audacious")
    -- -- Master and Stack Controls
    -- --, ("M-r", refresh)
    -- --, ("M-m", windows W.focusMaster  )
    -- , ("M-<Tab>", windows W.focusDown)
    -- , ("M-j", windows W.focusDown)
    -- , ("M-k", windows W.focusUp  )
    -- --, ("M-<Return>", windows W.swapMaster)
    -- , ("M-S-j", windows W.swapDown  )
    -- , ("M-S-k", windows W.swapUp    )
    -- , ("M-S-g", sendMessage $ SwapWindow)
    -- , ("M-h", sendMessage Shrink)
    -- , ("M-l", sendMessage Expand)
    -- , ("M-m", sendMessage MirrorExpand)
    -- , ("M-n", sendMessage MirrorShrink)
    -- , ("M-,", sendMessage (IncMasterN 1))
    -- , ("M-.", sendMessage (IncMasterN (-1)))
    -- Directional Movement Controls
    -- Switch between layers
    , ("M-S-<Space>", switchLayer)
    -- Directional navigation of windows
    , ("M-h", sendMessage $ WN.Go WN.L)
    , ("M-j", sendMessage $ WN.Go WN.D)
    , ("M-k", sendMessage $ WN.Go WN.U)
    , ("M-l", sendMessage $ WN.Go WN.R)
    , ("M-m", windows W.focusUp)
    , ("M-n", windows W.focusDown)
    -- Size controls
    , ("M-M1-h", sendMessage Shrink)
    , ("M-M1-l", sendMessage Expand)
    , ("M-M1-j", sendMessage MirrorExpand)
    , ("M-M1-k", sendMessage MirrorShrink)
    -- Swap adjacent windows
    , ("M-C-l", sendMessage $ WN.Swap WN.R)
    , ("M-C-h", sendMessage $ WN.Swap WN.L)
    , ("M-C-k", sendMessage $ WN.Swap WN.U)
    , ("M-C-j", sendMessage $ WN.Swap WN.D)
    -- -- Float keys
    -- , ("M-M1-<U>", withFocused (keysMoveWindow (0,-80)))
    -- , ("M-M1-<D>", withFocused (keysMoveWindow (0, 80)))
    -- , ("M-M1-<L>", withFocused (keysMoveWindow (-80,0)))
    -- , ("M-M1-<R>", withFocused (keysMoveWindow (80, 0)))
    -- , ("M-M1-k", withFocused (keysMoveWindow (0,-80)))
    -- , ("M-M1-j", withFocused (keysMoveWindow (0, 80)))
    -- , ("M-M1-h", withFocused (keysMoveWindow (-80,0)))
    -- , ("M-M1-l", withFocused (keysMoveWindow (80, 0)))
    -- -- Center the window
    -- --, ("M-c", withFocused (keysMoveWindowTo (1920,1080) (1%2, 1%2)))
    -- -- Float Snapping Keys
    -- , ("C-M-<L>", withFocused $ snapMove L Nothing)
    -- , ("C-M-<R>", withFocused $ snapMove R Nothing)
    -- , ("C-M-<U>", withFocused $ snapMove U Nothing)
    -- , ("C-M-<D>", withFocused $ snapMove D Nothing)
    -- , ("C-M1-h", withFocused $ snapMove L Nothing)
    -- , ("C-M1-l", withFocused $ snapMove R Nothing)
    -- , ("C-M1-k", withFocused $ snapMove U Nothing)
    -- , ("C-M1-j", withFocused $ snapMove D Nothing)
    -- , ("C-M-<L>", withFocused $ snapShrink R Nothing)
    -- , ("C-M-<R>", withFocused $ snapGrow R Nothing)
    -- , ("C-M-<U>", withFocused $ snapShrink D Nothing)
    -- , ("C-M-<D>", withFocused $ snapGrow D Nothing)
    -- , ("C-M-h", withFocused $ snapShrink R Nothing)
    -- , ("C-M-l", withFocused $ snapGrow R Nothing)
    -- , ("C-M-k", withFocused $ snapShrink D Nothing)
    -- , ("C-M-j", withFocused $ snapGrow D Nothing)
    -- Tags
    -- , ("M-f", withFocused (addTag "abc"))
    -- , ("C-M-f", withFocused (delTag "abc"))
    -- , ("S-M-f", withTaggedGlobalP "abc" W.sink)
    -- , ("M-d", withTaggedP "abc" (W.shiftWin "2"))
    -- , ("S-M-d", withTaggedGlobalP "abc" shiftHere)
    -- , ("C-M-d", focusUpTaggedGlobal "abc")
    -- , ("M-g", tagPrompt myXPConfig (\s -> withFocused (addTag s)))
    -- , ("C-M-g", tagDelPrompt def)
    -- , ("S-M-g", tagPrompt def (\s -> withTaggedGlobal s float))
    -- , ("M1-g", tagPrompt def (\s -> withTaggedP s (W.shiftWin "2")))
    -- , ("S-M1-g", tagPrompt def (\s -> withTaggedGlobalP s shiftHere))
    -- , ("C-M1-g", tagPrompt def (\s -> focusUpTaggedGlobal s))
    ]

------------------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

--    , ((modm .|. shiftMask, button1), dragWindow)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)

    -- Float Snapping Mouse Bindings
    -- , ((mod1Mask, button1), (\w -> focus w >> mouseMoveWindow w >> ifClick (snapMagicMove (Just 50) (Just 50) w)))
    -- , ((mod1Mask .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w >> ifClick (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)))
    -- , ((mod1Mask, button3), (\w -> focus w >> mouseResizeWindow w >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)))

    -- alternative mouse bindings
    --, ((mod1Mask,               button1), (\w -> focus w >> mouseMoveWindow w >> afterDrag (snapMagicMove (Just 50) (Just 50) w)))
    --, ((mod1Mask .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w >> afterDrag (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)))
    --, ((mod1Mask,               button3), (\w -> focus w >> mouseResizeWindow w >> afterDrag (snapMagicResize [R,D] (Just 50) (Just 50) w)))
    ]

------------------------------------------------------------------------

convertToBool' :: [Int] -> [Bool]
convertToBool' = map (== 1)

convertToBool :: [[Int]] -> [[Bool]]
convertToBool = map convertToBool'

menuButton' :: [[Int]]
menuButton' = [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]

menuButton :: [[Bool]]
menuButton = convertToBool menuButton'

miniButton' :: [[Int]]
miniButton' = [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]]

miniButton :: [[Bool]]
miniButton = convertToBool miniButton'

maxiButton' :: [[Int]]
maxiButton' = [[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]]

maxiButton :: [[Bool]]
maxiButton = convertToBool maxiButton'

closeButton' :: [[Int]]
closeButton' = [[1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
                [0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0],
                [0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0],
                [0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0],
                [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0],
                [0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0],
                [0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0],
                [0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0],
                [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0],
                [0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0],
                [0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0],
                [0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0],
                [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]]


closeButton :: [[Bool]]
closeButton = convertToBool closeButton'

buttonSize :: Int
buttonSize = 30

menuButtonOffset :: Int
menuButtonOffset = 40

maximizeButtonOffset :: Int
maximizeButtonOffset = 60

minimizeButtonOffset :: Int
minimizeButtonOffset = 200

closeButtonOffset :: Int
closeButtonOffset = 40

imageTitleBarButtonHandler :: Window -> Int -> Int -> X Bool
imageTitleBarButtonHandler mainw distFromLeft distFromRight = do
    let action
          | fi distFromLeft >= menuButtonOffset &&
             fi distFromLeft <= menuButtonOffset + buttonSize = focus mainw >> windowMenu >> return True
          | fi distFromRight >= closeButtonOffset &&
            fi distFromRight <= closeButtonOffset + buttonSize = focus mainw >> kill >> return True
          | fi distFromRight >= maximizeButtonOffset &&
            fi distFromRight <= maximizeButtonOffset + buttonSize = focus mainw >> sendMessage (maximizeRestore mainw) >> return True
          | fi distFromRight >= minimizeButtonOffset &&
            fi distFromRight <= minimizeButtonOffset + buttonSize = focus mainw >> minimizeWindow mainw >> return True
          | otherwise = return False
    action

defaultThemeWithImageButtons :: Theme
defaultThemeWithImageButtons = def
                               { fontName = "xft:M+ 1c:size=11"
                               , inactiveBorderColor = "#1e2731"
                               , inactiveColor = "#000507"
                               , inactiveTextColor = "#1e2731"
                               , inactiveBorderWidth = 1
                               , activeBorderColor = "#81a1c1"
                               , activeColor = "#000507"
                               , activeTextColor = "#81a1c1"
                               , activeBorderWidth = 1
                               , urgentBorderColor = "#bf616a"
                               , urgentColor = "#000507"
                               , urgentTextColor = "#bf616a"
                               , urgentBorderWidth = 1
                               , decoHeight = 70
                               , windowTitleIcons = [ (menuButton, CenterLeft 40),
                                                      (closeButton, CenterRight 40),
                                                      (maxiButton, CenterRight 120),
                                                      (miniButton, CenterRight 200) ]
                               }

imageButtonDeco :: (Eq a, Shrinker s) => s -> Theme
                   -> l a -> ModifiedLayout (Decoration ImageButtonDecoration s) l a
imageButtonDeco s c = decoration s c $ NFD True

newtype ImageButtonDecoration a = NFD Bool deriving (Show, Read)

instance Eq a => DecorationStyle ImageButtonDecoration a where
    describeDeco _ = "ImageButtonDeco"
    decorationCatchClicksHook _ mainw dFL dFR = imageTitleBarButtonHandler mainw dFL dFR
    decorationAfterDraggingHook _ (mainw, _) decoWin = focus mainw >> handleScreenCrossing mainw decoWin >> return ()

------------------------------------------------------------------------

myLayout = avoidStruts
         . (WN.configurableNavigation WN.noNavigateBorders)
         . smartBorders
         -- . fullScreenToggle
         . minimize
         . BW.boringWindows
         . maximize
         . ws1Layout
         . ws2Layout
         . ws3Layout
         . ws4Layout
         . windowDeco
         $ (bigGaps $ resizableTile)
       ||| (ifMax 2 (ifMax 1 (threeGapsSingle $ Full) (threeGapsDouble $ threeColumnMidDouble)) (threeGaps $ threeColumnMid))
       ||| (bigGaps $ Full)
       ||| (simplestFloat)
  where
     -- default tiling algorithm partitions the screen into two panes
     threeColumn = ThreeCol nmaster delta ratio
     threeColumnMid = ThreeColMid nmaster delta ratio
     threeColumnMidDouble = reflectHoriz $ (ThreeColMid nmaster delta (2/3))
     resizableTile = ResizableTall 1 (3/100) (1/2) []

     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

     -- Fullscreen
     -- fullScreenToggle = mkToggle (single NBFULL)

     -- Spacing
     -- top, bottom, right, left
     bigGaps   = spacingRaw False (Border 100 74 180 154)    True (Border 0 26 0 26) True
     smallGaps = spacingRaw False (Border 26 0 26 0)         True (Border 0 26 0 26) True
     terminalGaps = spacingRaw False (Border 300 274 860 834)   True (Border 0 26 0 26) True
     threeGapsSingle = spacingRaw False (Border 26 26 981 981)         True (Border 0 0 0 0) True
     threeGapsDouble = spacingRaw False (Border 26 0 981 0)         True (Border 0 26 0 26) True
     threeGaps = spacingRaw False (Border 26 0 26 0)         True (Border 0 26 0 26) True
     discordGaps = spacingRaw False (Border 300 274 450 424)   True (Border 0 26 0 26) True

     ws1Layout = onWorkspace ws1
       ((bigGaps $ Full)
       ||| (bigGaps $ resizableTile))
     ws2Layout = onWorkspace ws2
       ((bigGaps $ Full)
       ||| (bigGaps $ resizableTile))
     ws3Layout = onWorkspace ws3
       ((ifMax 2 (ifMax 1 (discordGaps $ Full) (bigGaps $ resizableTile)) (smallGaps $ resizableTile))
       ||| (windowDeco $ simplestFloat))
     ws4Layout = onWorkspace ws4
       ((ifMax 2 (ifMax 1 (threeGapsSingle $ Full) (threeGapsDouble $ threeColumnMidDouble)) (threeGaps $ threeColumnMid))
       ||| (threeGaps $ Full))

     windowDeco = imageButtonDeco shrinkText defaultThemeWithImageButtons

-- myLayout =  avoidStruts
--          . (WN.configurableNavigation WN.noNavigateBorders)
--          . smartBorders
--          . fullScreenToggle
--          . minimize
--          . BW.boringWindows
--          -- . maximizeWithPadding 50
--          $ (borderGaps $ resizableTile)
--        ||| ifMax 2 (ifMax 1 (threeGapsSingle $ Full) (threeGapsDouble $ threeColumnMidDouble)) (threeGaps $ threeColumnMid)
--        ||| (borderGaps $ Full)
--   where
--      threeColumn = ThreeCol nmaster delta ratio
--      threeColumnMid = ThreeColMid nmaster delta ratio
--      threeColumnMidDouble = reflectHoriz $ (ThreeColMid nmaster delta (2/3))
--      resizableTile = ResizableTall 1 (3/100) (1/2) []

--      nmaster = 1
--      ratio   = 1/2
--      delta   = 3/100

--      borderGaps = spacingRaw False (Border 100 0 100 80) True (Border 0 20 0 20) True
--      threeGapsSingle = spacingRaw False (Border 100 0 1010 990) True (Border 0 20 0 20) True
--      threeGapsDouble = spacingRaw False (Border 200 0 1010 80) True (Border 0 20 0 20) True
--      threeGaps = spacingRaw False (Border 100 0 100 80) True (Border 0 20 0 20) True
--      -- Fullscreen
--      fullScreenToggle = mkToggle (single NBFULL)

------------------------------------------------------------------------

myManageHook = composeAll
    [ insertPosition Master Newer -- open new windows below current window
    , className =? "MPlayer"                                          --> mediaFloat
    , className =? "mpv"                                              --> mediaFloat
    , className =? "vlc"                                              --> mediaFloat
    , className =? "io.github.celluloid_player.Celluloid"             --> mediaFloat
    , className =? "gwenview"                                         --> mediaFloat
    , className =? "Sxiv"                                             --> mediaFloat
    , className =? "Orage"                                            --> doFloat
    , className =? "Nemo"                                             --> myRectFloat
    , className =? "Gimp"                                             --> doFloat
    , className =? "Galculator"                                       --> calculatorFloat
    , className =? "Firefox" <&&> resource =? "Toolkit"               --> myRectFloat
    , className =? "chromium-browser" <&&> isDialog                   --> myRectFloat
    , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog"       --> myRectFloat
    , stringProperty "WM_WINDOW_ROLE" =? "pop-up"                     --> myRectFloat
    , isDialog                                                        --> myRectFloat
    , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH" --> myRectFloat
    , title     =? "Save Image"                                       --> myRectFloat
    , title     =? "Save File"                                        --> myRectFloat
    , title     =? "Open"                                             --> myRectFloat
    , title     =? "Open Files"                                       --> myRectFloat
    , title     =? "emacs-run-launcher"                               --> scratchpadFloat
    , resource  =? "xmomacs-help"                                     --> helpFloat
    , resource  =? "desktop_window"                                   --> doIgnore
    , resource  =? "kdesktop"                                         --> doIgnore
    , isFullscreen                                                    --> doFullFloat
    ]
  where
    unfloat = ask >>= doF . W.sink
    -- xpos, ypos, width, height
    myRectFloat = doRectFloat (W.RationalRect (1 % 3) (3 % 10) (1 % 3) (2 % 5))
    mediaFloat = doRectFloat (W.RationalRect (3 % 10) (3 % 20) (2 % 5) (7 % 10))
    calculatorFloat = doRectFloat (W.RationalRect (7 % 16) (2 % 6) (1 % 8) (1 % 3))
    scratchpadFloat = doRectFloat (W.RationalRect (1 % 3) (1 % 4) (1 % 3) (1 % 2))
    helpFloat = doRectFloat (W.RationalRect (7 % 8) (0 % 1) (1 % 8) (1 % 2))

------------------------------------------------------------------------

myEventHook = fullscreenEventHook

------------------------------------------------------------------------

myLogHook = return ()

------------------------------------------------------------------------

myStartupHook = do
        spawnOnce "emacs --daemon"
        spawnOnce "urxvtd --quiet &"
        spawnOnce "pcmanfm --daemon-mode &"
        spawnOnce "feh --bg-fill /etc/wallpaper.jpg"

------------------------------------------------------------------------

bar = "xmobar $HOME/.config/xmobar/bar"

ppWorkspaces = xmobarPP { ppCurrent = xmobarColor "#d8dee9" "" . wrap "<fc=#d8dee9,#81a1c1:0>" "</fc>"
                        , ppHidden = xmobarColor "#d8dee9" "" . clickable
                        , ppHiddenNoWindows = xmobarColor "#1e2731" "" . clickable
                        , ppVisible = xmobarColor "#d8dee9" "" . clickable
                        , ppTitle = xmobarColor "#d8dee9" ""
                        , ppOrder = \(ws:_:_:_) -> [ws]
                        }

-- Key binding to toggle the gap from the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

------------------------------------------------------------------------

launcherXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
launcherXPKeymap  = M.fromList
  [ ((controlMask, xK_z), killBefore)
  , ((controlMask, xK_k), killAfter)
  , ((controlMask, xK_a), startOfLine)
  , ((controlMask, xK_e), endOfLine)
  , ((controlMask, xK_m), deleteString Next)
  , ((controlMask, xK_b), moveCursor Prev)
  , ((controlMask, xK_f), moveCursor Next)
  , ((controlMask, xK_BackSpace), killWord Prev)
  , ((controlMask, xK_y), pasteString)
  , ((controlMask, xK_g), quit)
  , ((controlMask, xK_bracketleft), quit)
  , ((mod1Mask, xK_BackSpace), killWord Prev)
  , ((mod1Mask, xK_f), moveWord Next)
  , ((mod1Mask, xK_b), moveWord Prev)
  , ((mod1Mask, xK_d), killWord Next)
  , ((mod1Mask, xK_n), moveHistory W.focusUp')
  , ((mod1Mask, xK_p), moveHistory W.focusDown')
  , ((0, xK_Return), setSuccess True >> setDone True)
  , ((0, xK_KP_Enter), setSuccess True >> setDone True)
  , ((0, xK_BackSpace), deleteString Prev)
  , ((0, xK_Delete), deleteString Next)
  , ((0, xK_Left), moveCursor Prev)
  , ((0, xK_Right), moveCursor Next)
  , ((0, xK_Home), startOfLine)
  , ((0, xK_End), endOfLine)
  , ((0, xK_Down), moveHistory W.focusUp')
  , ((0, xK_Up), moveHistory W.focusDown')
  , ((0, xK_Escape), quit)
  ]

launcherXPConfig = def { font                = "xft:M+ 1c:size=11"
                       , bgColor             = "#000507"
                       , fgColor             = "#d8dee9"
                       , bgHLight            = "#0d1319"
                       , fgHLight            = "#bf616a"
                       , borderColor         = "#bf616a"
                       , promptBorderWidth   = 2
                       , position            = CenteredAt (471 % 480) (1 % 2)
                       , alwaysHighlight     = True
                       , height              = 90
                       , maxComplRows        = Just 14
                       , historySize         = 256
                       , historyFilter       = id
                       , promptKeymap        = launcherXPKeymap
                       , defaultText         = []
                       , autoComplete        = Nothing
                       , showCompletionOnTab = True
                       , searchPredicate     = fuzzyMatch
                       , sorter              = fuzzySort
                       }

------------------------------------------------------------------------

-- Xmonad Prefix Mode

-- Keybinds
prefixXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
prefixXPKeymap = M.fromList
  [ ((controlMask, xK_g), quit)
  , ((controlMask, xK_bracketleft), quit)
  , ((0, xK_Escape), quit)
  , ((0, xK_Return), setSuccess True >> setDone True)
  , ((0, xK_KP_Enter), setSuccess True >> setDone True)
  , ((0, xK_BackSpace), deleteString Prev)
  , ((0, xK_Delete), deleteString Next)
  --, ((0, xK_space), (shellPrompt launcherXPConfig))
  , ((0, xK_space), setSuccess True >> setDone True >> spawn "xdotool key super+x")
  , ((mod1Mask, xK_space), setSuccess True >> setDone True >> spawn "xdotool key super+x")
  , ((controlMask, xK_h), setSuccess True >> setDone True >> spawn "urxvtc -name xmomacs-help -e man xmonad")
  , ((controlMask, xK_r), setSuccess True >> setDone True >> spawn "xmonad --recompile; xmonad --restart")
  , ((controlMask, xK_q), io (exitWith ExitSuccess))
  ]

prefixXPConfig = def { font                = "xft:M+ 1c:size=11"
                     , bgColor             = "#000507"
                     , fgColor             = "#d8dee9"
                     , bgHLight            = "#0d1319"
                     , fgHLight            = "#a3be8c"
                     , borderColor         = "#a3be8c"
                     , promptBorderWidth = 2
                     , position = CenteredAt (471 % 480) (1 % 2)
                     , alwaysHighlight = False
                     , height = 90
                     , maxComplRows = Just 14
                     , historySize = 256
                     , historyFilter = id
                     , promptKeymap = prefixXPKeymap
                     , defaultText = []
                     , autoComplete = Just 0
                     , showCompletionOnTab = False
                     , searchPredicate = isPrefixOf
                     }

prefixCommands :: M.Map String (X ())
prefixCommands = fromList [
                          -- Search
                            (" ", shellPrompt launcherXPConfig)

                          -- Launch
                          , ("a", spawn "audacious")
                          , ("d", spawn "discord")
                          , ("e", spawn "emacsclient -c")
                          , ("E", spawn "emacs")
                          , ("f", spawn "firefox")
                          , ("F", spawn "pcmanfm --new-win")
                          , ("g", spawn "steam")
                          , ("t", spawn "urxvtc")

                          -- Commands
                          , ("q", kill)
                          , ("Q", spawn "xkill")
                          , ("c", sendMessage NextLayout)
                          , ("C", sendMessage FirstLayout)
                          , ("m", withFocused (sendMessage . maximizeRestore))
                          , ("s", spawn "flameshot full -p ~/Screenshots/")
                          , ("S", spawn "flameshot gui")
                          , ("M", sendMessage ToggleStruts)

                          -- Window management
                          -- , ("o", windows W.focusDown)
                          , ("h", sendMessage $ WN.Go WN.L)
                          , ("j", sendMessage $ WN.Go WN.D)
                          , ("k", sendMessage $ WN.Go WN.U)
                          , ("l", sendMessage $ WN.Go WN.R)
                          , ("n", sendMessage Shrink)
                          , ("m", sendMessage Expand)
                          , ("L", sendMessage $ WN.Swap WN.R)
                          , ("H", sendMessage $ WN.Swap WN.L)
                          , ("K", sendMessage $ WN.Swap WN.U)
                          , ("J", sendMessage $ WN.Swap WN.D)

                          -- Windows
                          , ("b", windowPrompt prefixXPConfig Goto allWindows)
                          , ("B", windowPrompt prefixXPConfig Bring allWindows)

                          -- Workspaces
                          -- , ("w", workspacePrompt prefixXPConfig (windows . W.greedyView))
                          -- , ("W", workspacePrompt prefixXPConfig (windows . W.shift))
                          , ("wt", sendMessage $ JumpToLayout "Tall")
                          , ("wn", nextWS)
                          , ("wp", prevWS)

                          , ("1", spawn "xdotool key super+1")
                          , ("2", spawn "xdotool key super+2")
                          , ("3", spawn "xdotool key super+3")
                          , ("4", spawn "xdotool key super+4")
                          , ("5", spawn "xdotool key super+5")
                          , ("6", spawn "xdotool key super+6")
                          , ("7", spawn "xdotool key super+7")
                          , ("8", spawn "xdotool key super+8")
                          , ("9", spawn "xdotool key super+9")
                          ]

runCommand :: String -> X ()
runCommand requestedCmd =
  case M.lookup requestedCmd prefixCommands of
    Nothing -> refresh --in future add error message
    Just (commandToExec) -> commandToExec

prefixPrompt :: X ()
prefixPrompt = inputPromptWithCompl prefixXPConfig "M-SPC" (mkComplFunFromList (M.keys prefixCommands)) ?+ runCommand

------------------------------------------------------------------------

-- Xmonad Command Mode

-- Keybinds
commandXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
commandXPKeymap  = M.fromList
  [ -- Basic typing controls
    ((controlMask, xK_z), killBefore)
  , ((controlMask, xK_k), killAfter)
  , ((controlMask, xK_a), startOfLine)
  , ((controlMask, xK_e), endOfLine)
  , ((controlMask, xK_m), deleteString Next)
  , ((controlMask, xK_b), moveCursor Prev)
  , ((controlMask, xK_f), moveCursor Next)
  , ((controlMask, xK_BackSpace), killWord Prev)
  , ((controlMask, xK_y), pasteString)
  , ((controlMask, xK_g), quit)
  , ((controlMask, xK_bracketleft), quit)
  , ((mod1Mask, xK_BackSpace), killWord Prev)
  , ((mod1Mask, xK_f), moveWord Next)
  , ((mod1Mask, xK_b), moveWord Prev)
  , ((mod1Mask, xK_d), killWord Next)
  , ((mod1Mask, xK_n), moveHistory W.focusUp')
  , ((mod1Mask, xK_p), moveHistory W.focusDown')
  , ((0, xK_Return), setSuccess True >> setDone True)
  , ((0, xK_KP_Enter), setSuccess True >> setDone True)
  , ((0, xK_BackSpace), deleteString Prev)
  , ((0, xK_Delete), deleteString Next)
  , ((0, xK_Left), moveCursor Prev)
  , ((0, xK_Right), moveCursor Next)
  , ((0, xK_Home), startOfLine)
  , ((0, xK_End), endOfLine)
  , ((0, xK_Down), moveHistory W.focusUp')
  , ((0, xK_Up), moveHistory W.focusDown')
  , ((0, xK_Escape), quit)
  ]

commandXPConfig = def { font                = "xft:M+ 1c:size=11"
                      , bgColor             = "#000507"
                      , fgColor             = "#d8dee9"
                      , bgHLight            = "#0d1319"
                      , fgHLight            = "#bf616a"
                      , borderColor         = "#bf616a"
                      , promptBorderWidth   = 2
                      , position = CenteredAt (471 % 480) (1 % 2)
                      , alwaysHighlight     = False
                      , height              = 90
                      , maxComplRows        = Just 14
                      , historySize         = 256
                      , historyFilter       = id
                      , promptKeymap        = commandXPKeymap
                      , defaultText         = []
                      , autoComplete        = Nothing
                      , showCompletionOnTab = True
                      , searchPredicate     = fuzzyMatch
                      , sorter              = fuzzySort
                      }

commandCommands :: M.Map String (X ())
-- Functions
commandCommands = fromList [
                    -- Audio
                      ("audio-toggle-mute", spawn "pamixer --toggle-mute")
                    , ("audio-music-forward", spawn "audacious --fwd")
                    , ("audio-music-rewind", spawn "audacious --rew")
                    , ("audio-music-toggle-pause", spawn "audacious --play-pause")
                    , ("audio-music-stop", spawn "audacious --stop")

                    -- Launch
                    , ("audacious", spawn "audacious")
                    , ("browser", spawn "firefox")
                    , ("discord", spawn "discord")
                    , ("emacs", spawn "emacsclient -c")
                    , ("file-manager", spawn "pcmanfm --new-win")
                    , ("steam", spawn "steam")
                    , ("terminal", spawn "urxvtc")

                    -- Commands
                    , ("xmonad-kill", kill)
                    , ("xkill", spawn "xkill")
                    , ("xmonad-next-layout", sendMessage NextLayout)
                    , ("xmonad-first-layout", sendMessage FirstLayout)
                    , ("screenshot-full", spawn "flameshot full -p ~/Screenshots/")
                    , ("screenshot-selection", spawn "flameshot gui")
                    , ("xmonad-exit", io (exitWith ExitSuccess))
                    ]

commandRunCommand :: String -> X ()
commandRunCommand requestedCmd =
  case M.lookup requestedCmd commandCommands of
    --Nothing -> refresh --in future add error message
    Nothing -> spawn $ requestedCmd
    Just (commandToExec) -> commandToExec

commandPrompt :: X ()
commandPrompt = inputPromptWithCompl commandXPConfig "m-x" (mkComplFunFromList (M.keys commandCommands)) ?+ commandRunCommand

------------------------------------------------------------------------

main = xmonad
       . ewmh
       . docks
        =<< statusBar bar ppWorkspaces toggleStrutsKey defaults

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
