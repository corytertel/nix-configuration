{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}
{-# OPTIONS_GHC -Wunused-imports #-}

import XMonad hiding ((|||))

import Data.Monoid (mappend)
import Data.Map (fromList, lookup)
import Data.Maybe (fromJust, isJust)
import Data.Ratio ((%)) -- for video

import Control.Monad
import Foreign.C.Types(CInt)

import Graphics.X11.ExtraTypes.XF86

import System.Exit

import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.Image
import XMonad.Util.PositionStore
import XMonad.Util.XUtils

import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.IfMax
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.BorderResize
import XMonad.Layout.Tabbed
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.SubLayouts
import XMonad.Layout.StateFull
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.WindowArranger
import XMonad.Layout.LayoutModifier

import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageHelpers
  (isFullscreen, isDialog,  doFullFloat, doCenterFloat, doRectFloat, composeOne, isInProperty)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.Minimize

import XMonad.Actions.Navigation2D (switchLayer)
import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap
import XMonad.Actions.Search
import XMonad.Actions.WindowMenu
import XMonad.Actions.Minimize
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (killAll)
import XMonad.Actions.CycleWindows

import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad

import qualified XMonad.StackSet as W
import qualified XMonad.Core as C
import qualified Data.Map as M
import qualified XMonad.Layout.WindowNavigation as WN
import qualified XMonad.Layout.BoringWindows as BW
import qualified XMonad.Prelude as P
import qualified XMonad.Util.ExtensibleState as XS

myTerminal = "urxvtc"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth = 16

myNormalBorderColor  = "#eeeeee"
myFocusedBorderColor = "#e8e8e8"

barWidth = 110

myModMask = mod4Mask

myWorkspaces = [ "Ⅰ", "Ⅱ", "Ⅲ", "Ⅳ", "Ⅴ" ]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_5]
        , (f, m) <- [(W.greedyView, 0), (W.shift, controlMask)]]

myAdditionalKeys :: [(String, X ())]
myAdditionalKeys =
    -- Xmonad prompt
    [ ("M-x", spawn "rofi -matching fuzzy -show drun -modi drun,run --icon-theme \"Tango\" show-icons")
    , ("M1-<Space>", spawn "rofi -matching fuzzy -show drun -modi drun,run -icon-theme \"Tango\" -show-icons")
    -- Resize prompt
    , ("M-r", resizePrompt)
    , ("M-S-r", resizePrompt)
    -- Float prompt
    , ("M-f", floatPrompt)
    , ("M-S-f", floatPrompt)
    -- Audio Controls
    , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master unmute 2%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master unmute 2%+")
    , ("<XF86AudioMute>", spawn "amixer -q set Master toggle")
    , ("<XF86AudioNext>", spawn "audacious --fwd")
    , ("<XF86AudioPrev>", spawn "audacious --rew")
    , ("<XF86AudioPlay>", spawn "audacious --play-pause")
    , ("<XF86AudioStop>", spawn "audacious --stop")
    -- Brightness
    , ("<XF86MonBrightnessUp>", spawn "xbrightness +5000")
    , ("<XF86MonBrightnessDown>", spawn "xbrightness -5000")
    -- Screenshots
    , ("<Print>", spawn "flameshot full -p ~/Screenshots/")
    -- Keyboard Layout
    , ("S-C-<Space>", spawn "/home/cory/manual_installs/layout_switch.sh")

    ----------------------------------------------------------------------
    --                            Audio                                 --
    ----------------------------------------------------------------------
    , ("C-. a a", runOrRaise "audacious" (className =? "Audacious"))
    , ("C-. a n", spawn "audacious --fwd")
    , ("C-. a p", spawn "audacious --rew")
    , ("C-. a t", spawn "audacious --play-pause")
    , ("C-. a s", spawn "audacious --stop")

    ----------------------------------------------------------------------
    --                            Launch                                --
    ----------------------------------------------------------------------
    , ("C-. <Space>", spawn "rofi -matching fuzzy -show drun -modi drun,run --icon-theme \"Tango\" show-icons")
    , ("C-. d", runOrRaise "discord" (className =? "discord"))
    , ("C-. e", spawn "emacsclient -c")
    , ("C-. C-e", spawn "emacs")
    , ("C-. w", spawn "firefox")
    , ("C-. C-w", spawn "pcmanfm-qt --new-window")
    , ("C-. C-g", runOrRaise "steam" (className =? "Steam"))
    , ("C-. t", spawn $ myTerminal)

    ----------------------------------------------------------------------
    --                           Commands                               --
    ----------------------------------------------------------------------
    , ("C-. s", spawn "flameshot full -p ~/Screenshots/")
    , ("C-. C-s", spawn "flameshot gui")
    , ("C-. C-m", sendMessage ToggleStruts)
    , ("C-. u", withFocused (sendMessage . maximizeRestore))
    , ("C-. i", withFocused minimizeWindow)
    , ("C-. C-i", withLastMinimized maximizeWindowAndFocus)
    , ("C-. M-r", spawn "xmonad --recompile; xmonad --restart")

    ----------------------------------------------------------------------
    --                     Basic Window Management                      --
    ----------------------------------------------------------------------
    , ("C-. b", sendMessage $ WN.Go WN.L)
    , ("C-. n", sendMessage $ WN.Go WN.D)
    , ("C-. p", sendMessage $ WN.Go WN.U)
    , ("C-. f", sendMessage $ WN.Go WN.R)
    , ("C-. C-b", sendMessage $ WN.Swap WN.L)
    , ("C-. C-n", sendMessage $ WN.Swap WN.D)
    , ("C-. C-p", sendMessage $ WN.Swap WN.U)
    , ("C-. C-f", sendMessage $ WN.Swap WN.R)

    ----------------------------------------------------------------------
    --                       Buffers (Windows)                          --
    ----------------------------------------------------------------------
    , ("C-. M-b", windowPrompt windowXPConfig Goto allWindows)  -- Goto buffers
    -- , ("C-. M-b", windowPrompt windowXPConfig Bring allWindows) -- Bring buffer
    , ("C-. k", kill)                                     -- Kill buffer
    -- , ("bD", killAll)                                      -- Kill every buffer
    , ("C-. g n", onGroup W.focusDown')                   -- Next buffer
    , ("C-. g p", onGroup W.focusUp')                     -- Prev buffer
    , ("C-. g C-n", windows W.focusDown)                  -- Next buffer alt
    , ("C-. g C-p", windows W.focusUp)                    -- Prev buffer alt
    -- , ("bsn", onGroup swapDown')                          -- Swap next buffer
    -- , ("bsp", onGroup swapUp')                            -- Swap prev buffer
    -- , ("bsN", windows W.swapDown)                         -- Swap next buffer alt
    -- , ("bsP", windows W.swapUp)                           -- Swap prev buffer alt
    , ("C-. g s", sendMessage Swap)                            -- Swap groups
    , ("C-. m b", sendMessage $ pullGroup L)              -- Merge left
    , ("C-. m n", sendMessage $ pullGroup D)              -- Merge down
    , ("C-. m p", sendMessage $ pullGroup U)              -- Merge up
    , ("C-. m f", sendMessage $ pullGroup R)              -- Merge right

    ----------------------------------------------------------------------
    --                           Workspaces                             --
    ----------------------------------------------------------------------
    -- , ("wb", sendMessage Balance)
    -- , ("wB", sendMessage Equalize)
    -- , ("wn", moveTo Next NonEmptyWS)
    -- , ("wp", moveTo Prev NonEmptyWS)
    -- , ("wN", nextWS)
    -- , ("wP", prevWS)
    , ("C-. o !", withFocused (sendMessage . MergeAll)) -- Window focus
    -- , ("C-. C-f", floatPrompt) -- Window float
    , ("C-. r", resizePrompt) -- Window resize
    , ("C-. C-r", sendMessage Rotate) -- Window rotate
    , ("C-. o #", withFocused (sendMessage . UnMerge)) -- Window split
    , ("C-. o @", withFocused (sendMessage . UnMerge) >>
                  sendMessage Rotate) -- Window split
    , ("C-. C-l", sendMessage NextLayout)
    -- , ("M-S-C-j",  sendMessage $ SplitShift Prev)
    -- , ("M-S-C-k",  sendMessage $ SplitShift Next)
    ]
    ++
    -- Workspace switching and buffer move to workspace
    [ (otherModMasks ++ [key], action tag)
    | (tag, key)  <- zip myWorkspaces "!@#$%"
    , (otherModMasks, action) <-
        [ ("C-. ", windows . W.greedyView) , ("C-. C-", windows . W.shift)]
    ]

------------------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $ []

------------------------------------------------------------------------

convertToBool' :: [Int] -> [Bool]
convertToBool' = map (== 1)

convertToBool :: [[Int]] -> [[Bool]]
convertToBool = map convertToBool'

menuButton' :: [[Int]]
menuButton' = [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]

menuButton :: [[Bool]]
menuButton = convertToBool menuButton'

miniButton' :: [[Int]]
miniButton' = [[0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
               [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
               [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0]]

miniButton :: [[Bool]]
miniButton = convertToBool miniButton'

maxiButton' :: [[Int]]
maxiButton' = [[0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
               [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
               [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0]]

maxiButton :: [[Bool]]
maxiButton = convertToBool maxiButton'

closeButton' :: [[Int]]
closeButton' = [[0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
                [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
                [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
                [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
                [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
                [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
                [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
                [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
                [1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1],
                [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
                [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
                [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
                [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
                [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
                [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
                [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
                [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0]]

closeButton :: [[Bool]]
closeButton = convertToBool closeButton'

buttonSize :: Int
buttonSize = 34

menuButtonOffset :: Int
menuButtonOffset = 30

maximizeButtonOffset :: Int
maximizeButtonOffset = 90

minimizeButtonOffset :: Int
minimizeButtonOffset = 150

closeButtonOffset :: Int
closeButtonOffset = 30

imageTitleBarButtonHandler :: Window -> Int -> Int -> X Bool
imageTitleBarButtonHandler mainw distFromLeft distFromRight = do
    let action
          | fi distFromLeft >= menuButtonOffset &&
             fi distFromLeft <= menuButtonOffset + buttonSize = focus mainw >> return True
          | fi distFromRight >= closeButtonOffset &&
            fi distFromRight <= closeButtonOffset + buttonSize = focus mainw >> kill >> return True
          | fi distFromRight >= maximizeButtonOffset &&
            fi distFromRight <= maximizeButtonOffset + buttonSize = focus mainw >> sendMessage (maximizeRestore mainw) >> return True
          | fi distFromRight >= minimizeButtonOffset &&
            fi distFromRight <= minimizeButtonOffset + buttonSize = focus mainw >> minimizeWindow mainw >> return True
          | otherwise = return False
    action

defaultThemeWithImageButtons :: Theme
defaultThemeWithImageButtons =
  def { fontName = "xft:NotoSans Nerd Font:size=11"
      , inactiveBorderColor = "#eeeeee"
      , inactiveColor = "#eeeeee"
      , inactiveTextColor = "#999999"
      , inactiveBorderWidth = 0
      , activeBorderColor = "#e8e8e8"
      , activeColor = "#e8e8e8"
      , activeTextColor = "#141404"
      , activeBorderWidth = 0
      , urgentBorderColor = "#e8e8e8"
      , urgentColor = "#e8e8e8"
      , urgentTextColor = "#e60909"
      , urgentBorderWidth = 0
      , decoHeight = 70 - myBorderWidth
      , windowTitleIcons = [ (menuButton, CenterLeft 30),
                             (closeButton, CenterRight 30),
                             (maxiButton, CenterRight 90),
                             (miniButton, CenterRight 150) ]
      }

imageButtonDeco :: (Eq a, Shrinker s) => s -> Theme
                   -> l a -> ModifiedLayout (Decoration ImageButtonDecoration s) l a
imageButtonDeco s c = decoration s c $ NFD True

newtype ImageButtonDecoration a = NFD Bool deriving (Show, Read)

instance Eq a => DecorationStyle ImageButtonDecoration a where
    describeDeco _ = "ImageButtonDeco"
    decorationCatchClicksHook _ mainw dFL dFR = imageTitleBarButtonHandler mainw dFL dFR
    decorationWhileDraggingHook _ ex ey (mainw, r) x y = handleDraggingInProgress ex ey (mainw, r) x y
    decorationAfterDraggingHook _ (mainw, _) decoWin = focus mainw >> handleScreenCrossing mainw decoWin >> return ()

handleDraggingInProgress :: CInt -> CInt -> (Window, Rectangle)
  -> XMonad.Position -> XMonad.Position -> X ()
handleDraggingInProgress ex ey (_, r) x y = withDisplay $ \dpy ->
    let dw = displayWidth  dpy (defaultScreen dpy)
        dh = displayHeight dpy (defaultScreen dpy)
        wx = x - (fi ex - rect_x r)
        wy = y - (fi ey - rect_y r)
        regionWidth = 5
        rect =
          if y <= regionWidth
          then
            Rectangle 0 0 ((fi dw) - barWidth) (fi dh)
          else if x <= regionWidth
               then
                 Rectangle 0 0 (((fi dw) - barWidth) `div` 2) (fi dh)
          else if x >= (((fi dw) - 1) - regionWidth - (fi barWidth))
               then
                 Rectangle (fi (((fi dw) - barWidth) `div` 2)) 0 (((fi dw) - barWidth) `div` 2) (fi dh)
          else
            Rectangle wx wy (rect_width  r) (rect_height r)
    in sendMessage $ SetGeometry rect

-- handleDraggingInProgress :: CInt -> CInt -> (Window, Rectangle)
--   -> XMonad.Position -> XMonad.Position -> X ()
-- handleDraggingInProgress ex ey (_, r) x y = withDisplay $ \dpy ->
--     let dw = displayWidth  dpy (defaultScreen dpy)
--         dh = displayHeight dpy (defaultScreen dpy)
--         wx = x - (fi ex - rect_x r)
--         wy = y - (fi ey - rect_y r)
--         regionWidth = 5
--     in if y <= regionWidth
--        then
--          -- sendMessage $ SetGeometry (Rectangle 0 0 (fi dw) ((fi dh) - barWidth))
--          withFocused (sendMessage . maximizeRestore)
--     else if x <= regionWidth
--          then
--            sendMessage $ SetGeometry (Rectangle 0 0 ((fi dw) `div` 2) ((fi dh) - barWidth))
--     else if wx >= (((fi dh) - 1) - regionWidth)
--          then
--            sendMessage $ SetGeometry (Rectangle (fi ((fi dw) `div` 2)) 0 ((fi dw) `div` 2) ((fi dh) - barWidth))
--     else
--       sendMessage $ SetGeometry (Rectangle wx wy (rect_width  r) (rect_height r))

-- sendLowerLeft :: X ()
-- sendLowerLeft = withDisplay $ \dpy ->
--                        let dw = displayWidth  dpy (defaultScreen dpy) - 1
--                            dh = displayHeight dpy (defaultScreen dpy) - 1
--                            wy = fi (((fi dh) `div` 2) - (barWidth `div` 2))
--                            wwh = ((fi dw) `div` 2)
--                            wht = ((fi dh) `div` 2) - (barWidth `div` 2)
--                            rect = Rectangle 0 wy wwh wht
--                        in sendMessage (SetGeometry rect)

-- sendLowerRight :: X ()
-- sendLowerRight = withDisplay $ \dpy ->
--                        let dw = displayWidth  dpy (defaultScreen dpy) - 1
--                            dh = displayHeight dpy (defaultScreen dpy) - 1
--                            wx = fi ((fi dw) `div` 2)
--                            wy = fi (((fi dh) `div` 2) - (barWidth `div` 2))
--                            wwh = ((fi dw) `div` 2)
--                            wht = ((fi dh) `div` 2) - (barWidth `div` 2)
--                            rect = Rectangle wx wy wwh wht
--                        in sendMessage (SetGeometry rect)


windowSwitcherDecorationWithImageButtons :: (Eq a, Shrinker s) => s -> Theme
  -> l a -> ModifiedLayout (Decoration ImageWindowSwitcherDecoration s) l a
windowSwitcherDecorationWithImageButtons s c = decoration s c $ IWSD True

data ImageWindowSwitcherDecoration a = IWSD Bool deriving (Show, Read)

instance Eq a => DecorationStyle ImageWindowSwitcherDecoration a where
    describeDeco _ = "ImageWindowSwitcherDeco"

    decorationCatchClicksHook (IWSD withButtons) mainw dFL dFR = if withButtons
      then imageTitleBarButtonHandler mainw dFL dFR
      else return False
    decorationWhileDraggingHook _ ex ey (mainw, r) x y = handleTiledDraggingInProgress ex ey (mainw, r) x y
    decorationAfterDraggingHook _ (mainw, _) decoWin =
      do focus mainw
         hasCrossed <- handleScreenCrossing mainw decoWin
         unless hasCrossed $
           do sendMessage $ DraggingStopped
              performWindowSwitching mainw

handleTiledDraggingInProgress ex ey (mainw, r) x y = do
    let rect = Rectangle (x - (fi ex - rect_x r))
                         (y - (fi ey - rect_y r))
                         (rect_width  r)
                         (rect_height r)
    sendMessage $ DraggingWindow mainw rect

performWindowSwitching :: Window -> X ()
performWindowSwitching win =
    withDisplay $ \d -> do
       root <- asks theRoot
       (_, _, selWin, _, _, _, _, _) <- io $ queryPointer d root
       ws <- gets windowset
       let allWindows = W.index ws
       -- do a little double check to be sure
       if (win `elem` allWindows) && (selWin `elem` allWindows)
            then do
                let allWindowsSwitched = map (switchEntries win selWin) allWindows
                let (ls, t:rs) = break (win ==) allWindowsSwitched
                let newStack = W.Stack t (reverse ls) rs
                windows $ W.modify' $ \_ -> newStack
            else return ()
    where
        switchEntries a b x
            | x == a    = b
            | x == b    = a
            | otherwise = x

------------------------------------------------------------------------

newtype SimpleFloat a = SF Dimension deriving (Show, Read)
instance LayoutClass SimpleFloat Window where
    description _ = "Float"
    doLayout (SF i) sc (W.Stack w l r) = do
        wrs <- mapM (getSize i sc) (w : reverse l ++ r)
        return (wrs, Nothing)

-- argurment is vertical offset for bar
getSize :: Dimension -> Rectangle -> Window -> X (Window,Rectangle)
getSize i (Rectangle rx ry _ _) w = do
  d  <- asks display
  bw <- asks (borderWidth . config)
  wa <- io $ getWindowAttributes d w
  withDisplay $ \dpy ->
    let dw = fi (displayWidth  dpy (defaultScreen dpy))
        dh = fi (displayHeight dpy (defaultScreen dpy))
        wh = fi (wa_width  wa) + (bw * 2)
        ht = fi (wa_height wa) + (bw * 2)
        nx = if rx == 0 then ((dw - (fi i)) - (fi wh)) `div` 2 else rx
        ny = if ry == 0 then (dh - (fi ht)) `div` 2 else ry
        x  =  max nx $ fi $ wa_x wa
        y  =  max ny $ fi $ wa_y wa
    in return (w, Rectangle x y wh ht)

------------------------------------------------------------------------

resizeWidth :: Dimension
resizeWidth = 32

-- TODO when mouse resize module is cleaned up, use these as local variables
-- Resize Width
rw :: Dimension
rw = resizeWidth
-- Offset Resize Width
orw :: XMonad.Position
orw = fi (rw `div` 2)

mouseResizeSE :: l a -> ModifiedLayout MouseResizeSE l a
mouseResizeSE = ModifiedLayout (MR_SE [])

newtype MouseResizeSE a = MR_SE [((a,Rectangle),Maybe a)]
instance Show (MouseResizeSE a) where show        _ = ""
instance Read (MouseResizeSE a) where readsPrec _ s = [(MR_SE [], s)]

instance LayoutModifier MouseResizeSE Window where
    redoLayout _       _ Nothing  wrs = return (wrs, Nothing)
    redoLayout (MR_SE st) _ (Just s) wrs
        | [] <- st  = initState    >>= \nst -> return (wrs, Just $ MR_SE nst)
        | otherwise = processState >>= \nst -> return (wrs, Just $ MR_SE nst)
        where
          wrs'         = wrs_to_state [] . filter (isInStack s . fst) $ wrs
          initState    = mapM createInputWindowSE wrs'
          processState = mapM_ (deleteInputWin . snd) st >> mapM createInputWindowSE wrs'

          inputRectangle (Rectangle x y wh ht) = Rectangle (x + fi wh - orw) (y + fi ht - orw) rw rw

          wrs_to_state rs ((w,r):xs)
              | ir `isVisible` rs = ((w,r),Just ir) : wrs_to_state (r:ir:rs) xs
              | otherwise         = ((w,r),Nothing) : wrs_to_state (r:   rs) xs
              where ir = inputRectangle r
          wrs_to_state _ [] = []

    handleMess (MR_SE s) m
        | Just e <- fromMessage m :: Maybe Event = handleResizeSE s e >> return Nothing
        | Just Hide             <- fromMessage m = releaseResources >> return (Just $ MR_SE [])
        | Just ReleaseResources <- fromMessage m = releaseResources >> return (Just $ MR_SE [])
        where releaseResources = mapM_ (deleteInputWin . snd) s
    handleMess _ _ = return Nothing

mouseResizeSW :: l a -> ModifiedLayout MouseResizeSW l a
mouseResizeSW = ModifiedLayout (MR_SW [])

newtype MouseResizeSW a = MR_SW [((a,Rectangle),Maybe a)]
instance Show (MouseResizeSW a) where show        _ = ""
instance Read (MouseResizeSW a) where readsPrec _ s = [(MR_SW [], s)]

instance LayoutModifier MouseResizeSW Window where
    redoLayout _       _ Nothing  wrs = return (wrs, Nothing)
    redoLayout (MR_SW st) _ (Just s) wrs
        | [] <- st  = initState    >>= \nst -> return (wrs, Just $ MR_SW nst)
        | otherwise = processState >>= \nst -> return (wrs, Just $ MR_SW nst)
        where
          wrs'         = wrs_to_state [] . filter (isInStack s . fst) $ wrs
          initState    = mapM createInputWindowSW wrs'
          processState = mapM_ (deleteInputWin . snd) st >> mapM createInputWindowSW wrs'

          inputRectangle (Rectangle x y wh ht) = Rectangle (x - orw) (y + fi ht - orw) rw rw

          wrs_to_state rs ((w,r):xs)
              | ir `isVisible` rs = ((w,r),Just ir) : wrs_to_state (r:ir:rs) xs
              | otherwise         = ((w,r),Nothing) : wrs_to_state (r:   rs) xs
              where ir = inputRectangle r
          wrs_to_state _ [] = []

    handleMess (MR_SW s) m
        | Just e <- fromMessage m :: Maybe Event = handleResizeSW s e >> return Nothing
        | Just Hide             <- fromMessage m = releaseResources >> return (Just $ MR_SW [])
        | Just ReleaseResources <- fromMessage m = releaseResources >> return (Just $ MR_SW [])
        where releaseResources = mapM_ (deleteInputWin . snd) s
    handleMess _ _ = return Nothing

mouseResizeNW :: l a -> ModifiedLayout MouseResizeNW l a
mouseResizeNW = ModifiedLayout (MR_NW [])

newtype MouseResizeNW a = MR_NW [((a,Rectangle),Maybe a)]
instance Show (MouseResizeNW a) where show        _ = ""
instance Read (MouseResizeNW a) where readsPrec _ s = [(MR_NW [], s)]

instance LayoutModifier MouseResizeNW Window where
    redoLayout _       _ Nothing  wrs = return (wrs, Nothing)
    redoLayout (MR_NW st) _ (Just s) wrs
        | [] <- st  = initState    >>= \nst -> return (wrs, Just $ MR_NW nst)
        | otherwise = processState >>= \nst -> return (wrs, Just $ MR_NW nst)
        where
          wrs'         = wrs_to_state [] . filter (isInStack s . fst) $ wrs
          initState    = mapM createInputWindowNW wrs'
          processState = mapM_ (deleteInputWin . snd) st >> mapM createInputWindowNW wrs'

          inputRectangle (Rectangle x y wh ht) = Rectangle (x - orw) (y - orw) rw rw

          wrs_to_state rs ((w,r):xs)
              | ir `isVisible` rs = ((w,r),Just ir) : wrs_to_state (r:ir:rs) xs
              | otherwise         = ((w,r),Nothing) : wrs_to_state (r:   rs) xs
              where ir = inputRectangle r
          wrs_to_state _ [] = []

    handleMess (MR_NW s) m
        | Just e <- fromMessage m :: Maybe Event = handleResizeNW s e >> return Nothing
        | Just Hide             <- fromMessage m = releaseResources >> return (Just $ MR_NW [])
        | Just ReleaseResources <- fromMessage m = releaseResources >> return (Just $ MR_NW [])
        where releaseResources = mapM_ (deleteInputWin . snd) s
    handleMess _ _ = return Nothing

mouseResizeNE :: l a -> ModifiedLayout MouseResizeNE l a
mouseResizeNE = ModifiedLayout (MR_NE [])

newtype MouseResizeNE a = MR_NE [((a,Rectangle),Maybe a)]
instance Show (MouseResizeNE a) where show        _ = ""
instance Read (MouseResizeNE a) where readsPrec _ s = [(MR_NE [], s)]

instance LayoutModifier MouseResizeNE Window where
    redoLayout _       _ Nothing  wrs = return (wrs, Nothing)
    redoLayout (MR_NE st) _ (Just s) wrs
        | [] <- st  = initState    >>= \nst -> return (wrs, Just $ MR_NE nst)
        | otherwise = processState >>= \nst -> return (wrs, Just $ MR_NE nst)
        where
          wrs'         = wrs_to_state [] . filter (isInStack s . fst) $ wrs
          initState    = mapM createInputWindowNE wrs'
          processState = mapM_ (deleteInputWin . snd) st >> mapM createInputWindowNE wrs'

          inputRectangle (Rectangle x y wh ht) = Rectangle (x + fi wh - orw) (y - orw) rw rw

          wrs_to_state rs ((w,r):xs)
              | ir `isVisible` rs = ((w,r),Just ir) : wrs_to_state (r:ir:rs) xs
              | otherwise         = ((w,r),Nothing) : wrs_to_state (r:   rs) xs
              where ir = inputRectangle r
          wrs_to_state _ [] = []

    handleMess (MR_NE s) m
        | Just e <- fromMessage m :: Maybe Event = handleResizeNE s e >> return Nothing
        | Just Hide             <- fromMessage m = releaseResources >> return (Just $ MR_NE [])
        | Just ReleaseResources <- fromMessage m = releaseResources >> return (Just $ MR_NE [])
        where releaseResources = mapM_ (deleteInputWin . snd) s
    handleMess _ _ = return Nothing

mouseResizeS :: l a -> ModifiedLayout MouseResizeS l a
mouseResizeS = ModifiedLayout (MR_S [])

newtype MouseResizeS a = MR_S [((a,Rectangle),Maybe a)]
instance Show (MouseResizeS a) where show        _ = ""
instance Read (MouseResizeS a) where readsPrec _ s = [(MR_S [], s)]

instance LayoutModifier MouseResizeS Window where
    redoLayout _       _ Nothing  wrs = return (wrs, Nothing)
    redoLayout (MR_S st) _ (Just s) wrs
        | [] <- st  = initState    >>= \nst -> return (wrs, Just $ MR_S nst)
        | otherwise = processState >>= \nst -> return (wrs, Just $ MR_S nst)
        where
          wrs'         = wrs_to_state [] . filter (isInStack s . fst) $ wrs
          initState    = mapM createInputWindowS wrs'
          processState = mapM_ (deleteInputWin . snd) st >> mapM createInputWindowS wrs'

          inputRectangle (Rectangle x y wh ht) = Rectangle (x + orw) (y + fi ht - orw) (fi wh - rw) rw

          wrs_to_state rs ((w,r):xs)
              | ir `isVisible` rs = ((w,r),Just ir) : wrs_to_state (r:ir:rs) xs
              | otherwise         = ((w,r),Nothing) : wrs_to_state (r:   rs) xs
              where ir = inputRectangle r
          wrs_to_state _ [] = []

    handleMess (MR_S s) m
        | Just e <- fromMessage m :: Maybe Event = handleResizeS s e >> return Nothing
        | Just Hide             <- fromMessage m = releaseResources >> return (Just $ MR_S [])
        | Just ReleaseResources <- fromMessage m = releaseResources >> return (Just $ MR_S [])
        where releaseResources = mapM_ (deleteInputWin . snd) s
    handleMess _ _ = return Nothing

mouseResizeN :: l a -> ModifiedLayout MouseResizeN l a
mouseResizeN = ModifiedLayout (MR_N [])

newtype MouseResizeN a = MR_N [((a,Rectangle),Maybe a)]
instance Show (MouseResizeN a) where show        _ = ""
instance Read (MouseResizeN a) where readsPrec _ s = [(MR_N [], s)]

instance LayoutModifier MouseResizeN Window where
    redoLayout _       _ Nothing  wrs = return (wrs, Nothing)
    redoLayout (MR_N st) _ (Just s) wrs
        | [] <- st  = initState    >>= \nst -> return (wrs, Just $ MR_N nst)
        | otherwise = processState >>= \nst -> return (wrs, Just $ MR_N nst)
        where
          wrs'         = wrs_to_state [] . filter (isInStack s . fst) $ wrs
          initState    = mapM createInputWindowN wrs'
          processState = mapM_ (deleteInputWin . snd) st >> mapM createInputWindowN wrs'

          inputRectangle (Rectangle x y wh ht) = Rectangle (x + orw) (y - orw) (fi wh - rw) rw

          wrs_to_state rs ((w,r):xs)
              | ir `isVisible` rs = ((w,r),Just ir) : wrs_to_state (r:ir:rs) xs
              | otherwise         = ((w,r),Nothing) : wrs_to_state (r:   rs) xs
              where ir = inputRectangle r
          wrs_to_state _ [] = []

    handleMess (MR_N s) m
        | Just e <- fromMessage m :: Maybe Event = handleResizeN s e >> return Nothing
        | Just Hide             <- fromMessage m = releaseResources >> return (Just $ MR_N [])
        | Just ReleaseResources <- fromMessage m = releaseResources >> return (Just $ MR_N [])
        where releaseResources = mapM_ (deleteInputWin . snd) s
    handleMess _ _ = return Nothing

mouseResizeE :: l a -> ModifiedLayout MouseResizeE l a
mouseResizeE = ModifiedLayout (MR_E [])

newtype MouseResizeE a = MR_E [((a,Rectangle),Maybe a)]
instance Show (MouseResizeE a) where show        _ = ""
instance Read (MouseResizeE a) where readsPrec _ s = [(MR_E [], s)]

instance LayoutModifier MouseResizeE Window where
    redoLayout _       _ Nothing  wrs = return (wrs, Nothing)
    redoLayout (MR_E st) _ (Just s) wrs
        | [] <- st  = initState    >>= \nst -> return (wrs, Just $ MR_E nst)
        | otherwise = processState >>= \nst -> return (wrs, Just $ MR_E nst)
        where
          wrs'         = wrs_to_state [] . filter (isInStack s . fst) $ wrs
          initState    = mapM createInputWindowE wrs'
          processState = mapM_ (deleteInputWin . snd) st >> mapM createInputWindowE wrs'

          inputRectangle (Rectangle x y wh ht) = Rectangle (x + fi wh - orw) (y + orw) rw (fi ht - rw)

          wrs_to_state rs ((w,r):xs)
              | ir `isVisible` rs = ((w,r),Just ir) : wrs_to_state (r:ir:rs) xs
              | otherwise         = ((w,r),Nothing) : wrs_to_state (r:   rs) xs
              where ir = inputRectangle r
          wrs_to_state _ [] = []

    handleMess (MR_E s) m
        | Just e <- fromMessage m :: Maybe Event = handleResizeE s e >> return Nothing
        | Just Hide             <- fromMessage m = releaseResources >> return (Just $ MR_E [])
        | Just ReleaseResources <- fromMessage m = releaseResources >> return (Just $ MR_E [])
        where releaseResources = mapM_ (deleteInputWin . snd) s
    handleMess _ _ = return Nothing

mouseResizeW :: l a -> ModifiedLayout MouseResizeW l a
mouseResizeW = ModifiedLayout (MR_W [])

newtype MouseResizeW a = MR_W [((a,Rectangle),Maybe a)]
instance Show (MouseResizeW a) where show        _ = ""
instance Read (MouseResizeW a) where readsPrec _ s = [(MR_W [], s)]

instance LayoutModifier MouseResizeW Window where
    redoLayout _       _ Nothing  wrs = return (wrs, Nothing)
    redoLayout (MR_W st) _ (Just s) wrs
        | [] <- st  = initState    >>= \nst -> return (wrs, Just $ MR_W nst)
        | otherwise = processState >>= \nst -> return (wrs, Just $ MR_W nst)
        where
          wrs'         = wrs_to_state [] . filter (isInStack s . fst) $ wrs
          initState    = mapM createInputWindowW wrs'
          processState = mapM_ (deleteInputWin . snd) st >> mapM createInputWindowW wrs'

          inputRectangle (Rectangle x y wh ht) = Rectangle (x - orw) (y + orw) rw (fi ht - rw)

          wrs_to_state rs ((w,r):xs)
              | ir `isVisible` rs = ((w,r),Just ir) : wrs_to_state (r:ir:rs) xs
              | otherwise         = ((w,r),Nothing) : wrs_to_state (r:   rs) xs
              where ir = inputRectangle r
          wrs_to_state _ [] = []

    handleMess (MR_W s) m
        | Just e <- fromMessage m :: Maybe Event = handleResizeW s e >> return Nothing
        | Just Hide             <- fromMessage m = releaseResources >> return (Just $ MR_W [])
        | Just ReleaseResources <- fromMessage m = releaseResources >> return (Just $ MR_W [])
        where releaseResources = mapM_ (deleteInputWin . snd) s
    handleMess _ _ = return Nothing

handleResizeSE :: [((Window,Rectangle),Maybe Window)] -> Event -> X ()
handleResizeSE st ButtonEvent { ev_window = ew, ev_event_type = et }
    | et == buttonPress
    , Just (w,Rectangle wx wy _ _) <- getWin ew st = do
        focus w
        mouseDrag (\x y ->
                     do
                       let rect = Rectangle wx wy
                                  (max 1 . fi $ x - wx)
                                  (max 1 . fi $ y - wy)
                       sendMessage (SetGeometry rect)) (return ())

      where
        getWin w (((win,r),tw):xs)
            | Just w' <- tw
            , w == w'   = Just (win,r)
            | otherwise = getWin w xs
        getWin _ []     = Nothing
handleResizeSE _ _ = return ()

handleResizeSW :: [((Window,Rectangle),Maybe Window)] -> Event -> X ()
handleResizeSW st ButtonEvent { ev_window = ew, ev_event_type = et }
    | et == buttonPress
    , Just (w,Rectangle wx wy wwh _) <- getWin ew st = do
        focus w
        mouseDrag (\x y ->
                     do
                       let rect = Rectangle (max 0 $ min (wx + fi wwh) x) wy
                                  (max 1 $ wwh + fi (wx - x))
                                  (max 1 . fi $ y - wy)
                       sendMessage (SetGeometry rect)) (return ())

      where
        getWin w (((win,r),tw):xs)
            | Just w' <- tw
            , w == w'   = Just (win,r)
            | otherwise = getWin w xs
        getWin _ []     = Nothing
handleResizeSW _ _ = return ()

handleResizeNW :: [((Window,Rectangle),Maybe Window)] -> Event -> X ()
handleResizeNW st ButtonEvent { ev_window = ew, ev_event_type = et }
    | et == buttonPress
    , Just (w,Rectangle wx wy wwh wht) <- getWin ew st = do
        focus w
        mouseDrag (\x y ->
                     do
                       let rect = Rectangle (max 0 $ min (wx + fi wwh) x)
                                  (max 0 $ min (wy + fi wht) y)
                                  (max 1 $ wwh + fi (wx - x))
                                  (max 1 $ wht + fi (wy - y))
                       sendMessage (SetGeometry rect)) (return ())

      where
        getWin w (((win,r),tw):xs)
            | Just w' <- tw
            , w == w'   = Just (win,r)
            | otherwise = getWin w xs
        getWin _ []     = Nothing
handleResizeNW _ _ = return ()

handleResizeNE :: [((Window,Rectangle),Maybe Window)] -> Event -> X ()
handleResizeNE st ButtonEvent { ev_window = ew, ev_event_type = et }
    | et == buttonPress
    , Just (w,Rectangle wx wy wwh wht) <- getWin ew st = do
        focus w
        mouseDrag (\x y ->
                     do
                       let rect = Rectangle wx (max 0 $ min (wy + fi wht) y)
                                  (max 1 . fi $ x - wx)
                                  (max 1 $ wht + fi (wy - y))
                       sendMessage (SetGeometry rect)) (return ())

      where
        getWin w (((win,r),tw):xs)
            | Just w' <- tw
            , w == w'   = Just (win,r)
            | otherwise = getWin w xs
        getWin _ []     = Nothing
handleResizeNE _ _ = return ()

handleResizeS :: [((Window,Rectangle),Maybe Window)] -> Event -> X ()
handleResizeS st ButtonEvent { ev_window = ew, ev_event_type = et }
    | et == buttonPress
    , Just (w,Rectangle wx wy wwh _) <- getWin ew st = do
        focus w
        mouseDrag (\x y ->
                     do
                       let rect = Rectangle wx wy wwh (max 1 . fi $ y - wy)
                       sendMessage (SetGeometry rect)) (return ())

      where
        getWin w (((win,r),tw):xs)
            | Just w' <- tw
            , w == w'   = Just (win,r)
            | otherwise = getWin w xs
        getWin _ []     = Nothing
handleResizeS _ _ = return ()

handleResizeN :: [((Window,Rectangle),Maybe Window)] -> Event -> X ()
handleResizeN st ButtonEvent { ev_window = ew, ev_event_type = et }
    | et == buttonPress
    , Just (w,Rectangle wx wy wwh wht) <- getWin ew st = do
        focus w
        mouseDrag (\x y ->
                     do
                       let rect = Rectangle wx (max 0 $ min (wy + fi wht) y) wwh (max 1 $ wht + fi (wy - y))
                       sendMessage (SetGeometry rect)) (return ())

      where
        getWin w (((win,r),tw):xs)
            | Just w' <- tw
            , w == w'   = Just (win,r)
            | otherwise = getWin w xs
        getWin _ []     = Nothing
handleResizeN _ _ = return ()

handleResizeE :: [((Window,Rectangle),Maybe Window)] -> Event -> X ()
handleResizeE st ButtonEvent { ev_window = ew, ev_event_type = et }
    | et == buttonPress
    , Just (w,Rectangle wx wy _ wht) <- getWin ew st = do
        focus w
        mouseDrag (\x y ->
                     do
                       let rect = Rectangle wx wy (max 1 . fi $ x - wx) wht
                       sendMessage (SetGeometry rect)) (return ())

      where
        getWin w (((win,r),tw):xs)
            | Just w' <- tw
            , w == w'   = Just (win,r)
            | otherwise = getWin w xs
        getWin _ []     = Nothing
handleResizeE _ _ = return ()

handleResizeW :: [((Window,Rectangle),Maybe Window)] -> Event -> X ()
handleResizeW st ButtonEvent { ev_window = ew, ev_event_type = et }
    | et == buttonPress
    , Just (w,Rectangle wx wy wwh wht) <- getWin ew st = do
        focus w
        mouseDrag (\x y ->
                     do
                       let rect = Rectangle (max 0 $ min (wx + fi wwh) x) wy (max 1 $ wwh + fi (wx - x)) wht
                       sendMessage (SetGeometry rect)) (return ())

      where
        getWin w (((win,r),tw):xs)
            | Just w' <- tw
            , w == w'   = Just (win,r)
            | otherwise = getWin w xs
        getWin _ []     = Nothing
handleResizeW _ _ = return ()

createInputWindowSE :: ((Window,Rectangle), Maybe Rectangle) -> X ((Window,Rectangle),Maybe Window)
createInputWindowSE ((w,r),mr) =
  case mr of
    Just tr  -> withDisplay $ \d -> do
                  tw <- mkInputWindow d tr
                  io $ selectInput d tw (exposureMask .|. buttonPressMask)

                  cursor <- io $ createFontCursor d xC_bottom_right_corner
                  io $ defineCursor d tw cursor
                  io $ freeCursor d cursor

                  showWindow tw
                  return ((w,r), Just tw)
    Nothing ->    return ((w,r), Nothing)

createInputWindowSW :: ((Window,Rectangle), Maybe Rectangle) -> X ((Window,Rectangle),Maybe Window)
createInputWindowSW ((w,r),mr) =
  case mr of
    Just tr  -> withDisplay $ \d -> do
                  tw <- mkInputWindow d tr
                  io $ selectInput d tw (exposureMask .|. buttonPressMask)

                  cursor <- io $ createFontCursor d xC_bottom_left_corner
                  io $ defineCursor d tw cursor
                  io $ freeCursor d cursor

                  showWindow tw
                  return ((w,r), Just tw)
    Nothing ->    return ((w,r), Nothing)

createInputWindowNW :: ((Window,Rectangle), Maybe Rectangle) -> X ((Window,Rectangle),Maybe Window)
createInputWindowNW ((w,r),mr) =
  case mr of
    Just tr  -> withDisplay $ \d -> do
                  tw <- mkInputWindow d tr
                  io $ selectInput d tw (exposureMask .|. buttonPressMask)

                  cursor <- io $ createFontCursor d xC_top_left_corner
                  io $ defineCursor d tw cursor
                  io $ freeCursor d cursor

                  showWindow tw
                  return ((w,r), Just tw)
    Nothing ->    return ((w,r), Nothing)

createInputWindowNE :: ((Window,Rectangle), Maybe Rectangle) -> X ((Window,Rectangle),Maybe Window)
createInputWindowNE ((w,r),mr) =
  case mr of
    Just tr  -> withDisplay $ \d -> do
                  tw <- mkInputWindow d tr
                  io $ selectInput d tw (exposureMask .|. buttonPressMask)

                  cursor <- io $ createFontCursor d xC_top_right_corner
                  io $ defineCursor d tw cursor
                  io $ freeCursor d cursor

                  showWindow tw
                  return ((w,r), Just tw)
    Nothing ->    return ((w,r), Nothing)

createInputWindowS :: ((Window,Rectangle), Maybe Rectangle) -> X ((Window,Rectangle),Maybe Window)
createInputWindowS ((w,r),mr) =
  case mr of
    Just tr  -> withDisplay $ \d -> do
                  tw <- mkInputWindow d tr
                  io $ selectInput d tw (exposureMask .|. buttonPressMask)

                  cursor <- io $ createFontCursor d xC_bottom_side
                  io $ defineCursor d tw cursor
                  io $ freeCursor d cursor

                  showWindow tw
                  return ((w,r), Just tw)
    Nothing ->    return ((w,r), Nothing)

createInputWindowN :: ((Window,Rectangle), Maybe Rectangle) -> X ((Window,Rectangle),Maybe Window)
createInputWindowN ((w,r),mr) =
  case mr of
    Just tr  -> withDisplay $ \d -> do
                  tw <- mkInputWindow d tr
                  io $ selectInput d tw (exposureMask .|. buttonPressMask)

                  cursor <- io $ createFontCursor d xC_top_side
                  io $ defineCursor d tw cursor
                  io $ freeCursor d cursor

                  showWindow tw
                  return ((w,r), Just tw)
    Nothing ->    return ((w,r), Nothing)

createInputWindowE :: ((Window,Rectangle), Maybe Rectangle) -> X ((Window,Rectangle),Maybe Window)
createInputWindowE ((w,r),mr) =
  case mr of
    Just tr  -> withDisplay $ \d -> do
                  tw <- mkInputWindow d tr
                  io $ selectInput d tw (exposureMask .|. buttonPressMask)

                  cursor <- io $ createFontCursor d xC_right_side
                  io $ defineCursor d tw cursor
                  io $ freeCursor d cursor

                  showWindow tw
                  return ((w,r), Just tw)
    Nothing ->    return ((w,r), Nothing)

createInputWindowW :: ((Window,Rectangle), Maybe Rectangle) -> X ((Window,Rectangle),Maybe Window)
createInputWindowW ((w,r),mr) =
  case mr of
    Just tr  -> withDisplay $ \d -> do
                  tw <- mkInputWindow d tr
                  io $ selectInput d tw (exposureMask .|. buttonPressMask)

                  cursor <- io $ createFontCursor d xC_left_side
                  io $ defineCursor d tw cursor
                  io $ freeCursor d cursor

                  showWindow tw
                  return ((w,r), Just tw)
    Nothing ->    return ((w,r), Nothing)

deleteInputWin :: Maybe Window -> X ()
deleteInputWin = maybe (return ()) deleteWindow

mkInputWindow :: Display -> Rectangle -> X Window
mkInputWindow d (Rectangle x y w h) = do
  rw <- asks theRoot
  let screen   = defaultScreenOfDisplay d
      visual   = defaultVisualOfScreen screen
      attrmask = cWOverrideRedirect
  io $ allocaSetWindowAttributes $
         \attributes -> do
           set_override_redirect attributes True
           createWindow d rw x y w h 0 0 inputOnly visual attrmask attributes

------------------------------------------------------------------------

-- for the future: want to spawn window within currently selected group,
-- and then if you want it to be separate unmerge it from the group.
-- the new window will already be selected so it will be easy to umerge it

-- Spacing
-- top, bottom, right, left
gaps = spacingRaw False (Border 100 74 180 154)
  True (Border 0 26 0 26) True

windowDeco = windowSwitcherDecorationWithImageButtons
             shrinkText defaultThemeWithImageButtons

floatingDeco = imageButtonDeco shrinkText defaultThemeWithImageButtons

emacs =
  renamed [Replace "bsp"] $
  (windowDeco . draggingVisualizer . (maximizeWithPadding 0)
   . subLayout [] StateFull . gaps $ emptyBSP)

floating =
  renamed [Replace "float"] $
  (floatingDeco . (maximizeWithPadding 0)
   . mouseResizeSE . mouseResizeSW . mouseResizeNW . mouseResizeNE
   -- . mouseResizeS . mouseResizeN . mouseResizeE . mouseResizeW
   . windowArrangeAll $ SF barWidth)

myLayout = avoidStruts
         . (WN.configurableNavigation WN.noNavigateBorders)
         . lessBorders OnlyScreenFloat
         . minimize
         . BW.boringWindows
         $ floating ||| emacs

------------------------------------------------------------------------

infix 0 -!>

-- | @p -!> x@.  If @p@ returns 'False', execute the 'ManageHook'.
--
-- > (-!>) :: Monoid m => Query Bool -> Query m -> Query m -- a simpler type
(-!>) :: (Monad m, Monoid a) => m Bool -> m a -> m a
p -!> f = p >>= \b -> if b then return mempty else f

-- | @q =? x@. if the result of @q@ equals @x@, return 'False'.
(=!?) :: Eq a => C.Query a -> a -> C.Query Bool
q =!? x = fmap (/= x) q

myManageHook = composeAll
    -- [ className =? "Orage"                                --> doCenterFloat
    [ className =? "Firefox" <&&> resource =? "Toolkit"   --> myRectFloat
    , stringProperty "WM_WINDOW_ROLE"
      =? "GtkFileChooserDialog"                           --> myRectFloat
    , stringProperty "WM_WINDOW_ROLE" =? "pop-up"         --> myRectFloat
    -- , isDialog                                            --> myRectFloat
    -- , isInProperty "_NET_WM_WINDOW_TYPE"
    --   "_NET_WM_WINDOW_TYPE_SPLASH"                        --> myRectFloat
    , title     =? "Save Image"                           --> myRectFloat
    , title     =? "Save File"                            --> myRectFloat
    , title     =? "Open"                                 --> myRectFloat
    , title     =? "Open Files"                           --> myRectFloat
    , resource  =? "desktop_window"                       --> doIgnore
    , resource  =? "kdesktop"                             --> doIgnore
    , isFullscreen --> doFullFloat
    , fmap not willFloat --> insertPosition Below Newer
    , fmap not willFloat -!> insertPosition Master Newer
    ]
  where
    -- xpos, ypos, width, height
    myRectFloat = doRectFloat (W.RationalRect (1 % 3) (3 % 10) (1 % 3) (2 % 5))
    helpFloat = doRectFloat (W.RationalRect (7 % 8) (0 % 1) (1 % 8) (1 % 2))

willFloat :: C.Query Bool
willFloat =
  ask >>= \w -> liftX $
    withDisplay $ \d -> do
      sh <- io $ getWMNormalHints d w
      let isFixedSize = isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
      isTransient <- isJust <$> io (getTransientForHint d w)
      return (isFixedSize || isTransient)

------------------------------------------------------------------------

myEventHook = minimizeEventHook

------------------------------------------------------------------------

myLogHook = return ()

------------------------------------------------------------------------

myStartupHook = do
  spawnOnce "emacs --daemon"
  spawnOnce "pcmanfm-qt --daemon-mode &"
  spawnOnce "pcmanfm-qt --desktop &"
  spawnOnce "urxvtd --quiet &"
  spawnOnce "sleep 4 && tint2 &"
  setWMName "LG3D"

------------------------------------------------------------------------

floatCommands :: [(String, X ())]
floatCommands =
  -- Float keys
  [ ("b", (withFocused (keysMoveWindow (0,-80))) >> spawn "xdotool key super+f")
  , ("n", (withFocused (keysMoveWindow (0, 80))) >> spawn "xdotool key super+f")
  , ("p", (withFocused (keysMoveWindow (-80,0))) >> spawn "xdotool key super+f")
  , ("f", (withFocused (keysMoveWindow (80, 0))) >> spawn "xdotool key super+f")
  -- Float Snapping Keys
  , ("sb", (withFocused $ snapMove L Nothing) >> spawn "xdotool key super+f")
  , ("sn", (withFocused $ snapMove D Nothing) >> spawn "xdotool key super+f")
  , ("sp", (withFocused $ snapMove U Nothing) >> spawn "xdotool key super+f")
  , ("sf", (withFocused $ snapMove R Nothing) >> spawn "xdotool key super+f")
  , ("sB", (withFocused $ snapShrink R Nothing) >> spawn "xdotool key super+f")
  , ("sN", (withFocused $ snapGrow D Nothing) >> spawn "xdotool key super+f")
  , ("sP", (withFocused $ snapShrink D Nothing) >> spawn "xdotool key super+f")
  , ("sF", (withFocused $ snapGrow R Nothing) >> spawn "xdotool key super+f")
  -- Push window back into tiling
  , ("t", (withFocused $ windows . W.sink) >> spawn "xdotool key super+f")
  -- Switch between layers
  , ("L", (switchLayer) >> spawn "xdotool key super+f")
  -- Center the window
  , ("c", (withFocused (keysMoveWindowTo (1920,1080) (1%2, 1%2)))
      >> spawn "xdotool key super+f")
  , ("q", return ())
  ]

floatPrompt :: X ()
floatPrompt = xmonadPromptC floatCommands baseXPConfig
              { fgHLight            = "#1f8c35"
              , borderColor         = "#1f8c35"
              }

------------------------------------------------------------------------

resizeCommands :: [(String, X ())]
resizeCommands =
  [ ("b", (sendMessage $ ExpandTowards L) >> spawn "xdotool key super+r")
  , ("n", (sendMessage $ ExpandTowards D) >> spawn "xdotool key super+r")
  , ("p", (sendMessage $ ExpandTowards U) >> spawn "xdotool key super+r")
  , ("f", (sendMessage $ ExpandTowards R) >> spawn "xdotool key super+r")
  , ("B", (sendMessage $ ShrinkFrom L) >> spawn "xdotool key super+r")
  , ("N", (sendMessage $ ShrinkFrom D) >> spawn "xdotool key super+r")
  , ("P", (sendMessage $ ShrinkFrom U) >> spawn "xdotool key super+r")
  , ("F", (sendMessage $ ShrinkFrom R) >> spawn "xdotool key super+r")
  , ("q", return ())
  ]

resizePrompt :: X ()
resizePrompt = xmonadPromptC resizeCommands baseXPConfig
               { fgHLight            = "#e01bd0"
               , borderColor         = "#e01bd0"
               }

------------------------------------------------------------------------

swapUp' :: W.Stack a -> W.Stack a
swapUp'  (W.Stack t (l:ls) rs) = W.Stack t ls (l:rs)
swapUp'  (W.Stack t []     rs) = W.Stack t (reverse rs) []

swapDown' :: W.Stack a -> W.Stack a
swapDown' = reverseStack . swapUp' . reverseStack

-- | reverse a stack: up becomes down and down becomes up.
reverseStack :: W.Stack a -> W.Stack a
reverseStack (W.Stack t ls rs) = W.Stack t rs ls

baseXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
baseXPKeymap = M.fromList
  [ ((controlMask, xK_g), quit)
  , ((controlMask, xK_bracketleft), quit)
  , ((0, xK_Escape), quit)
  , ((0, xK_Return), setSuccess True >> setDone True)
  , ((0, xK_KP_Enter), setSuccess True >> setDone True)
  , ((0, xK_BackSpace), deleteString Prev)
  , ((0, xK_Delete), deleteString Next)
  , ((controlMask, xK_h), setSuccess True >> setDone True
      >> spawn "urxvtc -name xmomacs-help -e man xmonad")
  , ((controlMask, xK_r), setSuccess True >> setDone True
      >> spawn "xmonad --recompile; xmonad --restart")
  , ((controlMask, xK_q), io (exitWith ExitSuccess))
  ]

baseXPConfig = def
  { font                = "xft:NotoSans Nerd Font:size=11"
  , bgColor             = "#e8e8e8"
  , fgColor             = "#141404"
  , bgHLight            = "#ffffff"
  , fgHLight            = "#ed8f23"
  , borderColor         = "#ed8f23"
  , promptBorderWidth = 2
  , position = CenteredAt (471 % 480) (1 % 2)
  , alwaysHighlight = False
  , height = 90
  , maxComplRows = Just 0
  , historySize = 256
  , historyFilter = id
  , promptKeymap = baseXPKeymap
  , completionKey = (0,xK_Tab)
  , defaultText = []
  , autoComplete = Just 0
  , showCompletionOnTab = False
  , searchPredicate = isPrefixOf
  }

windowXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
windowXPKeymap = M.fromList
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

windowXPConfig = baseXPConfig
  { promptKeymap        = windowXPKeymap
  , alwaysHighlight     = True
  , autoComplete        = Nothing
  , showCompletionOnTab = False
  , searchPredicate     = fuzzyMatch
  , sorter              = fuzzySort
  }

------------------------------------------------------------------------

main = xmonad . ewmhFullscreen . ewmh . docks $ defaults

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
