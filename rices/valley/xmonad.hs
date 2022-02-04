{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import XMonad

import Data.Monoid (mappend)
import Data.Map (fromList)
import Data.Maybe (fromJust)
import Data.Ratio ((%)) -- for video

import Graphics.X11.ExtraTypes.XF86

import System.Exit

import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig
import XMonad.Util.Image

import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
--import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
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
import XMonad.Layout.LayoutCombinators ((*|*))
import XMonad.Layout.ComboP

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

import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Layout.WindowNavigation as WN
import qualified XMonad.Layout.BoringWindows as BW

myTerminal      = "urxvtc -icon $HOME/.icons/icons/48x48/terminal.png"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 0

myNormalBorderColor  = "#999999"
myFocusedBorderColor = "#30343f"

myModMask       = mod4Mask

ws1 = "\61728 "
ws2 = "\62057 "
ws3 = "\62074 "
ws4 = "\61729 "
ws5 = "\61564 "
ws6 = "\61878 "
ws7 = "\61441 "
ws8 = "\61704 "
ws9 = "\61612 "

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
    [ ("M-<Return>", spawn $ myTerminal)
    -- launch emacs
    , ("M-S-<Return>", spawn "emacsclient -c")
    -- Xmonad prompt
    --, ("M-<Space>", shellPrompt myXPConfig)
    , ("M-<Space>", spawn "rofi -matching normal -show drun -modi drun,run -show-icons")
    --, ("M-S-<Space>", shellPrompt myXPConfig)
    -- File Manager
    , ("M-e", spawn "pcmanfm --new-win")
    --, ("M-e", spawn "nemo")
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
    , ("M-S-z", spawn "xscreensaver-command -lock")
    -- Screenshot
    , ("M-<Print>", spawn "flameshot full -p ~/Screenshots/")
    , ("M-S-<Print>", spawn "flameshot gui")
    -- Fullscreen
    --, ("M-f", sendMessage (Toggle NBFULL))
    -- Minimize
    , ("M-i", withFocused minimizeWindow)
    , ("M-S-i", withLastMinimized maximizeWindowAndFocus)
    -- Maximize
    , ("M-f", withFocused (sendMessage . maximizeRestore))
    -- Window Menu
    , ("M-o", windowMenu)
    -- Scratchpads
    , ("M-'", namedScratchpadAction myScratchpads "terminal")
    , ("M-0", namedScratchpadAction myScratchpads "audacious")
    -- Master and Stack Controls
    , ("M-r", refresh)
    --, ("M-m", windows W.focusMaster  )
    , ("M-<Tab>", windows W.focusDown)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp  )
    --, ("M-<Return>", windows W.swapMaster)
    , ("M-S-j", windows W.swapDown  )
    , ("M-S-k", windows W.swapUp    )
    , ("M-S-g", sendMessage $ SwapWindow)
    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)
    , ("M-m", sendMessage MirrorExpand)
    , ("M-n", sendMessage MirrorShrink)
    , ("M-,", sendMessage (IncMasterN 1))
    , ("M-.", sendMessage (IncMasterN (-1)))
    -- -- Binary Space Partition Controls
    -- , ("M-M1-l", sendMessage $ ExpandTowards WN.R)
    -- , ("M-M1-h", sendMessage $ ExpandTowards WN.L)
    -- , ("M-M1-j", sendMessage $ ExpandTowards WN.D)
    -- , ("M-M1-k", sendMessage $ ExpandTowards WN.U)
    -- , ("M-M1-C-l", sendMessage $ ShrinkFrom WN.R)
    -- , ("M-M1-C-h", sendMessage $ ShrinkFrom WN.L)
    -- , ("M-M1-C-j", sendMessage $ ShrinkFrom WN.D)
    -- , ("M-M1-C-k", sendMessage $ ShrinkFrom WN.U)
    -- , ("M-r", sendMessage Rotate)
    -- , ("M-s", sendMessage Swap)
    -- , ("M-,", sendMessage FocusParent)
    -- , ("M-C-,", sendMessage SelectNode)
    -- , ("M-S-,", sendMessage MoveNode)
    -- , ("M-S-C-j", sendMessage $ SplitShift Prev)
    -- , ("M-S-C-k", sendMessage $ SplitShift Next)
    -- , ("M-v",     sendMessage Balance)
    -- , ("M-S-v",     sendMessage Equalize)
    -- -- Switch between layers
    -- , ("M-S-<Space>", switchLayer)
    -- -- Directional navigation of windows
    -- , ("M-h", sendMessage $ WN.Go WN.L)
    -- , ("M-j", sendMessage $ WN.Go WN.D)
    -- , ("M-k", sendMessage $ WN.Go WN.U)
    -- , ("M-l", sendMessage $ WN.Go WN.R)
    -- , ("M-m", windows W.focusUp)
    -- , ("M-n", windows W.focusDown)
    -- -- Swap adjacent windows
    -- , ("M-C-l", sendMessage $ WN.Swap WN.R)
    -- , ("M-C-h", sendMessage $ WN.Swap WN.L)
    -- , ("M-C-k", sendMessage $ WN.Swap WN.U)
    -- , ("M-C-j", sendMessage $ WN.Swap WN.D)
    -- Float keys
    , ("M-M1-<U>", withFocused (keysMoveWindow (0,-80)))
    , ("M-M1-<D>", withFocused (keysMoveWindow (0, 80)))
    , ("M-M1-<L>", withFocused (keysMoveWindow (-80,0)))
    , ("M-M1-<R>", withFocused (keysMoveWindow (80, 0)))
    , ("M-M1-k", withFocused (keysMoveWindow (0,-80)))
    , ("M-M1-j", withFocused (keysMoveWindow (0, 80)))
    , ("M-M1-h", withFocused (keysMoveWindow (-80,0)))
    , ("M-M1-l", withFocused (keysMoveWindow (80, 0)))
    -- , ("M-<U>-<L>", withFocused (keysMoveWindow (-40,-40)))
    -- , ("M-<U>-<R>", withFocused (keysMoveWindow ( 40,-40)))
    -- , ("M-<D>-<L>", withFocused (keysMoveWindow (-40, 40)))
    -- , ("M-<D>-<R>", withFocused (keysMoveWindow ( 40, 40)))
    -- , ("M-M1-<U>", withFocused (keysResizeWindow (0,40) (1,0)))
    -- , ("M-M1-<D>", withFocused (keysResizeWindow (0,40) (0,0)))
    -- , ("M-M1-<L>", withFocused (keysResizeWindow (40,0) (1,0)))
    -- , ("M-M1-<R>", withFocused (keysResizeWindow (40,0) (0,0)))
    -- , ("M-M1-C-<U>", withFocused (keysResizeWindow (0,-40) (1,0)))
    -- , ("M-M1-C-<D>", withFocused (keysResizeWindow (0,-40) (0,0)))
    -- , ("M-M1-C-<L>", withFocused (keysResizeWindow (-40,0) (1,0)))
    -- , ("M-M1-C-<R>", withFocused (keysResizeWindow (-40,0) (0,0)))
    -- , ("M-S-<L>", withFocused (keysAbsResizeWindow (-40,-40) (2048,1504)))
    -- , ("M-S-<R>", withFocused (keysAbsResizeWindow ( 40, 40) (2048,1504)))
    -- Center the window
    , ("M-c", withFocused (keysMoveWindowTo (1920,1080) (1%2, 1%2)))
    -- Float Snapping Keys
    , ("C-M-<L>", withFocused $ snapMove L Nothing)
    , ("C-M-<R>", withFocused $ snapMove R Nothing)
    , ("C-M-<U>", withFocused $ snapMove U Nothing)
    , ("C-M-<D>", withFocused $ snapMove D Nothing)
    , ("C-M1-h", withFocused $ snapMove L Nothing)
    , ("C-M1-l", withFocused $ snapMove R Nothing)
    , ("C-M1-k", withFocused $ snapMove U Nothing)
    , ("C-M1-j", withFocused $ snapMove D Nothing)
    , ("C-M-<L>", withFocused $ snapShrink R Nothing)
    , ("C-M-<R>", withFocused $ snapGrow R Nothing)
    , ("C-M-<U>", withFocused $ snapShrink D Nothing)
    , ("C-M-<D>", withFocused $ snapGrow D Nothing)
    , ("C-M-h", withFocused $ snapShrink R Nothing)
    , ("C-M-l", withFocused $ snapGrow R Nothing)
    , ("C-M-k", withFocused $ snapShrink D Nothing)
    , ("C-M-j", withFocused $ snapGrow D Nothing)
    ]

------------------------------------------------------------------------

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

    -- Float Snapping Mouse Bindings
    , ((mod1Mask, button1), (\w -> focus w >> mouseMoveWindow w >> ifClick (snapMagicMove (Just 50) (Just 50) w)))
    , ((mod1Mask .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w >> ifClick (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)))
    , ((mod1Mask, button3), (\w -> focus w >> mouseResizeWindow w >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)))
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
menuButton' = [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]

menuButton :: [[Bool]]
menuButton = convertToBool menuButton'

miniButton' :: [[Int]]
miniButton' = [[0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
               [0,0,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0,0,0,0],
               [0,0,0,0,0,1,1,1,0,0,0,0,1,1,1,0,0,0,0,0],
               [0,0,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0,0,0,0],
               [0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0],
               [1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1],
               [1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1],
               [1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,1],
               [1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,1],
               [0,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,0],
               [0,1,1,1,1,0,0,0,1,1,1,1,0,0,0,1,1,1,1,0],
               [0,0,1,1,1,1,0,0,1,1,1,1,0,0,1,1,1,1,0,0],
               [0,0,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1,1,0,0],
               [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
               [0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0],
               [0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0]]

miniButton :: [[Bool]]
miniButton = convertToBool miniButton'

maxiButton' :: [[Int]]
maxiButton' = [[0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1],
               [0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1],
               [0,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1],
               [1,1,1,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1],
               [1,1,1,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,1,1],
               [1,1,1,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0],
               [1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,1,1,0,0,0],
               [1,1,1,0,0,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0],
               [1,1,1,0,0,0,0,1,1,1,0,0,0,0,1,1,1,0,0,0],
               [1,1,1,0,0,0,1,1,1,0,0,0,0,0,1,1,1,0,0,0],
               [1,1,1,0,0,0,1,1,0,0,0,0,0,0,1,1,1,0,0,0],
               [1,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0],
               [1,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0],
               [1,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0],
               [1,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0]]

maxiButton :: [[Bool]]
maxiButton = convertToBool maxiButton'

closeButton' :: [[Int]]
closeButton' = [[0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
                [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1],
                [1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1],
                [1,1,1,0,0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,1],
                [1,1,1,0,0,1,1,1,0,0,0,0,1,1,1,0,0,1,1,1],
                [1,1,1,0,0,0,1,1,1,0,0,1,1,1,0,0,0,1,1,1],
                [1,1,1,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1],
                [1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,1],
                [1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,1],
                [1,1,1,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1],
                [1,1,1,0,0,0,1,1,1,0,0,1,1,1,0,0,0,1,1,1],
                [1,1,1,0,0,1,1,1,0,0,0,0,1,1,1,0,0,1,1,1],
                [1,1,1,0,0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,1],
                [1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1],
                [1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
                [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0]]


closeButton :: [[Bool]]
closeButton = convertToBool closeButton'

buttonSize :: Int
buttonSize = 20

menuButtonOffset :: Int
menuButtonOffset = 20

maximizeButtonOffset :: Int
maximizeButtonOffset = 60

minimizeButtonOffset :: Int
minimizeButtonOffset = 100

closeButtonOffset :: Int
closeButtonOffset = 20

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
                               , inactiveBorderColor = "#aaaaaa"
                               , inactiveColor = "#30343f"
                               , inactiveTextColor = "#aaaaaa"
                               , inactiveBorderWidth = 0
                               , activeBorderColor = "#30343f"
                               , activeColor = "#e96179"
                               , activeTextColor = "#30343f"
                               , activeBorderWidth = 0
                               , urgentBorderColor = "#30343f"
                               , urgentColor = "#99cd61"
                               , urgentTextColor = "#30343f"
                               , urgentBorderWidth = 0
                               , decoHeight = 60
                               , windowTitleIcons = [ (menuButton, CenterLeft 20),
                                                      (closeButton, CenterRight 20),
                                                      (maxiButton, CenterRight 60),
                                                      (miniButton, CenterRight 100) ]
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
         . WN.windowNavigation
         . smartBorders
         -- . fullScreenToggle
         . minimize
         . BW.boringWindows
         . maximizeWithPadding 50
         . ws1Layout
         . ws2Layout
         . ws3Layout
         . ws4Layout
         . windowDeco
         $ (ifMax 2 (ifMax 1 (terminalGaps $ Full) (bigGaps $ resizableTile)) (smallGaps $ resizableTile))
       ||| (ifMax 2 (ifMax 1 (terminalGaps $ Full) (bigGaps $ threeColumnMid)) (smallGaps $ threeColumnMid))
       ||| (bigGaps $ resizableTile)
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
     bigGaps = spacingRaw False (Border 100 50 180 130) True (Border 0 50 0 50) True
     smallGaps = spacingRaw False (Border 50 0 50 0) True (Border 0 50 0 50) True
     terminalGaps = spacingRaw False (Border 300 250 860 810) True (Border 0 50 0 50) True
     terminal2GapsLeft = spacingRaw False (Border 150 150 50 300) True (Border 0 0 0 0) True
     terminal2GapsRight = spacingRaw False (Border 500 500 300 50) True (Border 0 0 0 0) True
     terminal3Gaps = spacingRaw False (Border 150 50 300 200) True (Border 0 100 0 100) True
     threeGapsSingle = spacingRaw False (Border 26 26 981 981) True (Border 0 0 0 0) True
     threeGapsDouble = spacingRaw False (Border 26 0 981 0) True (Border 0 26 0 26) True
     threeGaps = spacingRaw False (Border 26 0 26 0) True (Border 0 26 0 26) True
     discordGaps = spacingRaw False (Border 350 300 800 750) True (Border 0 50 0 50) True
     ws1Layout = onWorkspace ws1
       --(windowDeco $ (ifMax 2 (ifMax 1 (terminalGaps $ Full) ((terminal2GapsLeft $ Full) *|* (terminal2GapsRight $ Full))) (terminal3Gaps $ resizableTile))
       --(windowDeco $ (ifMax 2 (ifMax 1 (terminalGaps $ Full) (combineTwo resizableTile (terminal2GapsLeft $ Full) (terminal2GapsRight $ Full))) (terminal3Gaps $ resizableTile))
       (windowDeco $ (ifMax 2 (ifMax 1 (terminalGaps $ Full) (combineTwoP resizableTile (terminal2GapsLeft $ Full) (terminal2GapsRight $ Full) (ClassName "Emacs"))) (terminal3Gaps $ resizableTile))
       ||| (bigGaps $ Full))
     ws2Layout = onWorkspace ws2
       (windowDeco $ (ifMax 2 (bigGaps $ resizableTile) (smallGaps $ resizableTile))
       ||| (simplestFloat))
     ws3Layout = onWorkspace ws3
       (windowDeco $ (ifMax 2 (ifMax 1 (discordGaps $ Full) (bigGaps $ resizableTile)) (smallGaps $ resizableTile))
       ||| (simplestFloat))
     ws4Layout = onWorkspace ws4
       (windowDeco $ (ifMax 2 (ifMax 1 (threeGapsSingle $ Full) (threeGapsDouble $ threeColumnMidDouble)) (threeGaps $ threeColumnMid))
       ||| (threeGaps $ Full))
     windowDeco = imageButtonDeco shrinkText defaultThemeWithImageButtons

------------------------------------------------------------------------

myManageHook = composeAll
    [ insertPosition Below Newer -- open new windows below current window
    , className =? "MPlayer"                                          --> mediaFloat
    , className =? "mpv"                                              --> mediaFloat
    , className =? "vlc"                                              --> mediaFloat
    , className =? "Io.github.celluloid_player.Celluloid"             --> mediaFloat
    , className =? "gwenview"                                         --> mediaFloat
    , className =? "Sxiv"                                             --> mediaFloat
    , className =? "Orage"                                            --> doFloat
    --, className =? "Pcmanfm"                                          --> doFloat
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
    , resource  =? "desktop_window"                                   --> doIgnore
    , resource  =? "kdesktop"                                         --> doIgnore
    , isFullscreen                                                    --> doFullFloat
    ] <+> namedScratchpadManageHook myScratchpads
  where
    unfloat = ask >>= doF . W.sink
    -- xpos, ypos, width, height
    myRectFloat = doRectFloat (W.RationalRect (1 % 3) (3 % 10) (1 % 3) (2 % 5))
    mediaFloat = doRectFloat (W.RationalRect (3 % 10) (3 % 20) (2 % 5) (7 % 10))
    calculatorFloat = doRectFloat (W.RationalRect (7 % 16) (2 % 6) (1 % 8) (1 % 3))

------------------------------------------------------------------------

myEventHook = fullscreenEventHook

------------------------------------------------------------------------

myLogHook = return ()

------------------------------------------------------------------------

myStartupHook = do
        spawnOnce "emacs --daemon"
        spawnOnce "urxvtd --quiet &"
        spawnOnce "pcmanfm --daemon-mode &"
        spawnOnce "tint2 &"
        spawnOnce "feh --bg-fill /etc/wallpaper.jpg"

------------------------------------------------------------------------

myBar0 = "xmobar $HOME/.config/xmobar/xmobarrc0"
myBar1 = "xmobar $HOME/.config/xmobar/xmobarrc1"
myBar2 = "xmobar $HOME/.config/xmobar/xmobarrc2"
myBar3 = "xmobar $HOME/.config/xmobar/xmobarrc3"

myPP = xmobarPP { ppCurrent = xmobarColor "#e96179" "" . wrap "" "" --current selected desktop
                , ppHidden = xmobarColor "#30343f" "" . wrap "" "" . clickable
                , ppHiddenNoWindows = xmobarColor "#707577" "" . wrap "" "" . clickable --desktops with no windows
                , ppVisible = xmobarColor "#30343f" "" . wrap "" "" . clickable
                , ppTitle = xmobarColor "#30343f" "" . shorten 40
                , ppOrder = \(ws:_:_:_) -> [ws]
                }

myPP2 = xmobarPP { ppOrder = \(_:_:_:_) -> [] }

-- Key binding to toggle the gap from the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

------------------------------------------------------------------------

myXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
myXPKeymap  = M.fromList
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

myXPConfig = def { font = "xft:M+ 1c:size=12"
                 , bgColor = "#fefefe"
                 , fgColor = "#30343f"
                 , bgHLight = "#dddddd"
                 , fgHLight = "#fec7cd"
                 , borderColor = "#30343f"
                 , promptBorderWidth = 0
                 , promptKeymap = myXPKeymap
                 , position = CenteredAt (1 % 2) (1 % 4)
                 , height = 160
                 , historySize = 256
                 , historyFilter = id
                 , defaultText = []
                 , autoComplete = Nothing
                 , showCompletionOnTab = True
                 , searchPredicate = isPrefixOf
                 , alwaysHighlight = True
                 , maxComplRows = Nothing
                 }

------------------------------------------------------------------------

myScratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "audacious" spawnAudacious findAudacious manageAudacious
                ]
  where
    spawnTerm  = myTerminal ++ " -name scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect (1 % 3) (1 % 4) (1 % 3) (1 % 2)
    spawnAudacious  = "audacious"
    findAudacious   = resource =? "audacious"
    manageAudacious = customFloating $ W.RationalRect (1 % 3) (1 % 4) (1 % 3) (1 % 2)

------------------------------------------------------------------------

main = xmonad . ewmh =<< statusBar myBar1 myPP toggleStrutsKey =<< statusBar myBar0 myPP2 toggleStrutsKey =<< statusBar myBar2 myPP2 toggleStrutsKey =<< statusBar myBar3 myPP2 toggleStrutsKey defaults

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
