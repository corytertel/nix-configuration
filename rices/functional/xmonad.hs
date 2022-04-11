{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wunused-imports #-}

import XMonad hiding ((|||))

import Data.Monoid (mappend)
import Data.Map (fromList, lookup)
import Data.Maybe (fromJust, isJust)
import Data.Ratio ((%)) -- for video

import Control.Exception (IOException, catch)
import Control.Monad
import Foreign.C.Types(CInt)

import Graphics.X11.ExtraTypes.XF86

import System.Exit
import System.Environment (getEnv, setEnv)

import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.Image
import XMonad.Util.PositionStore
import XMonad.Util.XUtils

import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.MultiToggle (mkToggle, single, Toggle (..))
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
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.SubLayouts
import XMonad.Layout.StateFull
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.WindowArranger

import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageHelpers
  (isFullscreen, isDialog,  doFullFloat, doCenterFloat, doRectFloat, composeOne, isInProperty)
import XMonad.Hooks.SetWMName (setWMName)

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

myTerminal = "urxvtc"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth = 16

myNormalBorderColor  = "#eeeeee"
myFocusedBorderColor = "#e8e8e8"

myModMask = mod4Mask

myWorkspaces = [ "  dev  ", "  web  ", "  com  ", "  mus  ", "  etc  " ]

myWorkspaceIndices = M.fromList
  $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
  where i = fromJust $ M.lookup ws myWorkspaceIndices

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_5]
        , (f, m) <- [(W.greedyView, 0), (W.shift, controlMask)]]

myAdditionalKeys :: [(String, X ())]
myAdditionalKeys =
    -- Xmonad prompt
    [ ("M-x", shellPrompt launcherXPConfig)
    , ("M-<Space>", shellPrompt launcherXPConfig)
    , ("M1-<Space>", prefixPrompt)
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
    -- Keyboard Layout
    , ("S-M1-<Space>", spawn "/home/cory/manual_installs/layout_switch.sh")
    ]

------------------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                          >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                          >> windows W.shiftMaster))
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
miniButton' = [[0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
               [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
               [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0]]

miniButton :: [[Bool]]
miniButton = convertToBool miniButton'

maxiButton' :: [[Int]]
maxiButton' = [[0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
               [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
               [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0]]

maxiButton :: [[Bool]]
maxiButton = convertToBool maxiButton'

closeButton' :: [[Int]]
closeButton' = [[0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
                [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
                [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
                [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
                [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
                [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
                [0,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0],
                [0,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,0],
                [1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1],
                [0,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,0],
                [0,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0],
                [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
                [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
                [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
                [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
                [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
                [0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0]]

closeButton :: [[Bool]]
closeButton = convertToBool closeButton'

buttonSize :: Int
buttonSize = 30

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
             fi distFromLeft <= menuButtonOffset + buttonSize = focus mainw >> windowMenu >> return True
          | fi distFromRight >= closeButtonOffset &&
            fi distFromRight <= closeButtonOffset + buttonSize = focus mainw >> kill >> return True
          | fi distFromRight >= maximizeButtonOffset &&
            fi distFromRight <= maximizeButtonOffset + buttonSize = focus mainw >> sendMessage (Toggle NBFULL) >> return True
          | fi distFromRight >= minimizeButtonOffset &&
            fi distFromRight <= minimizeButtonOffset + buttonSize = focus mainw >> minimizeWindow mainw >> return True
          | otherwise = return False
    action

defaultThemeWithImageButtons :: Theme
defaultThemeWithImageButtons =
  def { fontName = "xft:mplus Nerd Font,M+ 1c:size=11"
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
    decorationAfterDraggingHook _ (mainw, _) decoWin = focus mainw >> handleScreenCrossing mainw decoWin >> return ()

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
        wrs <- mapM (getSize i i sc) (w : reverse l ++ r)
        return (wrs, Nothing)

getSize :: Dimension -> Dimension -> Rectangle -> Window -> X (Window,Rectangle)
getSize i j (Rectangle rx ry _ _) w = do
  d  <- asks display
  bw <- asks (borderWidth . config)
  wa <- io $ getWindowAttributes d w
  let nx = rx + fi j
      ny = ry + fi i
      x  =  max nx $ fi $ wa_x wa
      y  =  max ny $ fi $ wa_y wa
      wh = fi (wa_width  wa) + (bw * 2)
      ht = fi (wa_height wa) + (bw * 2)
  return (w, Rectangle x y wh ht)

------------------------------------------------------------------------

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

          inputRectangle (Rectangle x y wh ht) = Rectangle (x + fi wh - 20) (y + fi ht - 20) 40 40

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

          inputRectangle (Rectangle x y wh ht) = Rectangle (x - 20) (y + fi ht - 20) 40 40

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

          inputRectangle (Rectangle x y wh ht) = Rectangle (x - 20) (y - 20) 40 40

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

          inputRectangle (Rectangle x y wh ht) = Rectangle (x + fi wh - 20) (y - 20) 40 40

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

          inputRectangle (Rectangle x y wh ht) = Rectangle (x + 20) (y + fi ht - 20) (fi wh - 40) 40

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

          inputRectangle (Rectangle x y wh ht) = Rectangle (x + 20) (y - 20) (fi wh - 40) 40

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

          inputRectangle (Rectangle x y wh ht) = Rectangle (x + fi wh - 20) (y + 20) 40 (fi ht - 40)

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

          inputRectangle (Rectangle x y wh ht) = Rectangle (x - 20) (y + 20) 40 (fi ht - 40)

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
bigGaps = spacingRaw False (Border 100 74 180 154)
  True (Border 0 26 0 26) True

windowDeco = windowSwitcherDecorationWithImageButtons
             shrinkText defaultThemeWithImageButtons

floatingDeco = imageButtonDeco shrinkText defaultThemeWithImageButtons

emacs =
  renamed [Replace "bsp"] $
  (windowDeco . draggingVisualizer
   . subLayout [] StateFull . bigGaps $ emptyBSP)

floating =
  renamed [Replace "float"] $
  (floatingDeco $ mouseResizeSE $ mouseResizeSW $ mouseResizeNW
   $ mouseResizeNE $ mouseResizeS $ mouseResizeN $ mouseResizeE
   $ mouseResizeW $ windowArrangeAll $ SF 200)

myLayout = avoidStruts
         . (WN.configurableNavigation WN.noNavigateBorders)
         . lessBorders OnlyScreenFloat
         . fullScreenToggle
         . minimize
         . BW.boringWindows
         $ emacs ||| floating
  where
     fullScreenToggle = mkToggle (single NBFULL)

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
    [ className =? "MPlayer"                              --> mediaFloat
    , className =? "mpv"                                  --> mediaFloat
    , className =? "vlc"                                  --> mediaFloat
    , className =? "gwenview"                             --> mediaFloat
    , className =? "Sxiv"                                 --> mediaFloat
    , className =? "Orage"                                --> doCenterFloat
    , className =? "Galculator"                           --> calculatorFloat
    , className =? "Firefox" <&&> resource =? "Toolkit"   --> myRectFloat
    , stringProperty "WM_WINDOW_ROLE"
      =? "GtkFileChooserDialog"                           --> myRectFloat
    , stringProperty "WM_WINDOW_ROLE" =? "pop-up"         --> doCenterFloat
    , stringProperty "WM_WINDOW_ROLE" =!? "gimp-image-window-1"
      <&&> className =? "Gimp"                            --> doCenterFloat
    , stringProperty "WM_WINDOW_ROLE" =!? "MainWindow#1"
      <&&> className =? "krita"                           --> doCenterFloat
    , isDialog                                            --> doCenterFloat
    , isInProperty "_NET_WM_WINDOW_TYPE"
      "_NET_WM_WINDOW_TYPE_SPLASH"                        --> doCenterFloat
    , title     =? "Save Image"                           --> myRectFloat
    , title     =? "Save File"                            --> myRectFloat
    , title     =? "Open"                                 --> myRectFloat
    , title     =? "Open Files"                           --> myRectFloat
    , resource  =? "xmomacs-help"                         --> helpFloat
    , resource  =? "desktop_window"                       --> doIgnore
    , resource  =? "kdesktop"                             --> doIgnore
    , isFullscreen --> doFullFloat
    , fmap not willFloat --> insertPosition Below Newer
    , fmap not willFloat -!> insertPosition Master Newer
    ]
  where
    unfloat = ask >>= doF . W.sink
    -- xpos, ypos, width, height
    myRectFloat = doRectFloat (W.RationalRect (1 % 3) (3 % 10) (1 % 3) (2 % 5))
    mediaFloat = doRectFloat (W.RationalRect (3 % 10) (3 % 20) (2 % 5) (7 % 10))
    calculatorFloat = doRectFloat (W.RationalRect (7 % 16) (2 % 6) (1 % 8) (1 % 3))
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

myEventHook = fullscreenEventHook

------------------------------------------------------------------------

myLogHook = return ()

------------------------------------------------------------------------

myStartupHook = do
        spawnOnce "emacs --daemon"
        spawnOnce "urxvtd --quiet &"
        spawnOnce "pcmanfm --daemon-mode &"
        spawnOnce "feh --bg-fill /etc/wallpaper.jpg"
        setWMName "LG3D"

------------------------------------------------------------------------

bar = "xmobar $HOME/.config/xmobar/xmobarrc"

ppWorkspaces = xmobarPP
  { ppCurrent = xmobarColor "#141404" ""
    . wrap "<fc=#ffffff,#141404:10>" "</fc>"
  , ppHidden = xmobarColor "#141404" ""
    . wrap "<fc=#141404,#d8d8d8:10>" "</fc>" . clickable
  , ppHiddenNoWindows = xmobarColor "#777777" ""
    . wrap "<fc=#999999,#d8d8d8:10>" "</fc>" . clickable
  , ppVisible = xmobarColor "#141404" ""
    . wrap "<fc=#141404,#d8d8d8:10>" "</fc>" . clickable
  , ppUrgent = xmobarColor "#141404" ""
    . wrap "<fc=#141404,#ed8f23:10>" "</fc>" . clickable
  , ppOrder = \(ws:_:_:_) -> [ws]
  }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

------------------------------------------------------------------------

launcherXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
launcherXPKeymap = M.fromList
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

launcherXPConfig = def { font                = "xft:mplus Nerd Font,M+ 1c:size=11"
                       , bgColor             = "#e8e8e8"
                       , fgColor             = "#141404"
                       , bgHLight            = "#ffffff"
                       , fgHLight            = "#3647d9"
                       , borderColor         = "#3647d9"
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
                       , showCompletionOnTab = False
                       , searchPredicate     = fuzzyMatch
                       , sorter              = fuzzySort
                       }

------------------------------------------------------------------------

floatCommands :: [(String, X ())]
floatCommands =
  -- Float keys
  [ ("k", (withFocused (keysMoveWindow (0,-80))) >> spawn "xdotool key super+f")
  , ("j", (withFocused (keysMoveWindow (0, 80))) >> spawn "xdotool key super+f")
  , ("h", (withFocused (keysMoveWindow (-80,0))) >> spawn "xdotool key super+f")
  , ("l", (withFocused (keysMoveWindow (80, 0))) >> spawn "xdotool key super+f")
  -- Float Snapping Keys
  , ("sh", (withFocused $ snapMove L Nothing) >> spawn "xdotool key super+f")
  , ("sj", (withFocused $ snapMove D Nothing) >> spawn "xdotool key super+f")
  , ("sk", (withFocused $ snapMove U Nothing) >> spawn "xdotool key super+f")
  , ("sl", (withFocused $ snapMove R Nothing) >> spawn "xdotool key super+f")
  , ("sH", (withFocused $ snapShrink R Nothing) >> spawn "xdotool key super+f")
  , ("sJ", (withFocused $ snapGrow D Nothing) >> spawn "xdotool key super+f")
  , ("sK", (withFocused $ snapShrink D Nothing) >> spawn "xdotool key super+f")
  , ("sL", (withFocused $ snapGrow R Nothing) >> spawn "xdotool key super+f")
  -- Push window back into tiling
  , ("t", (withFocused $ windows . W.sink) >> spawn "xdotool key super+f")
  -- Switch between layers
  , ("L", (switchLayer) >> spawn "xdotool key super+f")
  -- Center the window
  , ("c", (withFocused (keysMoveWindowTo (1920,1080) (1%2, 1%2)))
      >> spawn "xdotool key super+f")
  , ("q", refresh)
  ]

floatPrompt :: X ()
floatPrompt = xmonadPromptC floatCommands prefixXPConfig
              { fgHLight            = "#1f8c35"
              , borderColor         = "#1f8c35"
              }

------------------------------------------------------------------------

resizeCommands :: [(String, X ())]
resizeCommands =
  [ ("h", (sendMessage $ ExpandTowards L) >> spawn "xdotool key super+r")
  , ("j", (sendMessage $ ExpandTowards D) >> spawn "xdotool key super+r")
  , ("k", (sendMessage $ ExpandTowards U) >> spawn "xdotool key super+r")
  , ("l", (sendMessage $ ExpandTowards R) >> spawn "xdotool key super+r")
  , ("H", (sendMessage $ ShrinkFrom L) >> spawn "xdotool key super+r")
  , ("J", (sendMessage $ ShrinkFrom D) >> spawn "xdotool key super+r")
  , ("K", (sendMessage $ ShrinkFrom U) >> spawn "xdotool key super+r")
  , ("L", (sendMessage $ ShrinkFrom R) >> spawn "xdotool key super+r")
  , ("q", refresh)
  ]

resizePrompt :: X ()
resizePrompt = xmonadPromptC resizeCommands prefixXPConfig
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

-- toggleLayout :: X ()
-- toggleLayout = do
  -- -- var <- catch (getEnv "LAYOUT") (const $ pure "none" :: IOException -> IO String)
  -- if var == "bsp"
  --   then
  --   do
  --     setEnv "LAYOUT" "float"
  --     return (sendMessage $ JumpToLayout "float")
  --   else
  --   do
  --     setEnv "LAYOUT" "bsp"
  --     return (sendMessage $ JumpToLayout "bsp")

prefixXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
prefixXPKeymap = M.fromList
  [ ((controlMask, xK_g), quit)
  , ((controlMask, xK_bracketleft), quit)
  , ((0, xK_Escape), quit)
  , ((0, xK_Return), setSuccess True >> setDone True)
  , ((0, xK_KP_Enter), setSuccess True >> setDone True)
  , ((0, xK_BackSpace), deleteString Prev)
  , ((0, xK_Delete), deleteString Next)
  , ((0, xK_space), setSuccess True >> setDone True
      >> spawn "xdotool key super+x")
  , ((controlMask, xK_h), setSuccess True >> setDone True
      >> spawn "urxvtc -name xmomacs-help -e man xmonad")
  , ((controlMask, xK_r), setSuccess True >> setDone True
      >> spawn "xmonad --recompile; xmonad --restart")
  , ((controlMask, xK_q), io (exitWith ExitSuccess))
  ]

prefixXPConfig = def
  { font                = "xft:mplus Nerd Font,M+ 1c:size=11"
  , bgColor             = "#e8e8e8"
  , fgColor             = "#141404"
  , bgHLight            = "#ffffff"
  , fgHLight            = "#ed8f23"
  , borderColor         = "#ed8f23"
  , promptBorderWidth = 2
  , position = CenteredAt (471 % 480) (1 % 2)
  , alwaysHighlight = False
  , height = 90
  , maxComplRows = Just 14
  , historySize = 256
  , historyFilter = id
  , promptKeymap = prefixXPKeymap
  , completionKey = (0,xK_Tab)
  , defaultText = []
  , autoComplete = Just 0
  , showCompletionOnTab = False
  , searchPredicate = isPrefixOf
  }

windowXPConfig = prefixXPConfig
  { promptKeymap        = launcherXPKeymap
  , alwaysHighlight     = True
  , autoComplete        = Nothing
  , showCompletionOnTab = False
  , searchPredicate     = fuzzyMatch
  , sorter              = fuzzySort
  }

prefixCommands :: [(String, X ())]
prefixCommands =
  ----------------------------------------------------------------------
  --                            Audio                                 --
  ----------------------------------------------------------------------
  [ ("aa", runOrRaise "audacious" (className =? "Audacious"))
  , ("an", spawn "audacious --fwd")
  , ("ap", spawn "audacious --rew")
  , ("at", spawn "audacious --play-pause")
  , ("as", spawn "audacious --stop")

  ----------------------------------------------------------------------
  --                            Launch                                --
  ----------------------------------------------------------------------
  , ("d", runOrRaise "discord" (className =? "discord"))
  , ("e", spawn "emacsclient -c")
  , ("E", spawn "emacs")
  , ("f", spawn "firefox")
  , ("F", spawn "pcmanfm --new-win")
  , ("g", runOrRaise "steam" (className =? "Steam"))
  , ("t", spawn $ myTerminal)

  ----------------------------------------------------------------------
  --                           Commands                               --
  ----------------------------------------------------------------------
  , ("s", spawn "flameshot full -p ~/Screenshots/")
  , ("S", spawn "flameshot gui")
  , ("M", sendMessage ToggleStruts)
  , ("u", sendMessage (Toggle NBFULL))
  , ("i", withFocused minimizeWindow)
  , ("I", withLastMinimized maximizeWindowAndFocus)

  ----------------------------------------------------------------------
  --                     Basic Window Management                      --
  ----------------------------------------------------------------------
  , ("h", sendMessage $ WN.Go WN.L)
  , ("j", sendMessage $ WN.Go WN.D)
  , ("k", sendMessage $ WN.Go WN.U)
  , ("l", sendMessage $ WN.Go WN.R)
  , ("H", sendMessage $ WN.Swap WN.L)
  , ("J", sendMessage $ WN.Swap WN.D)
  , ("K", sendMessage $ WN.Swap WN.U)
  , ("L", sendMessage $ WN.Swap WN.R)

  ----------------------------------------------------------------------
  --                       Buffers (Windows)                          --
  ----------------------------------------------------------------------
  , ("bb", windowPrompt windowXPConfig Goto allWindows)  -- Goto buffers
  , ("bB", windowPrompt windowXPConfig Bring allWindows) -- Bring buffer
  , ("bd", kill)                                         -- Kill buffer
  , ("bD", killAll)                                      -- Kill every buffer
  , ("bn", onGroup W.focusDown')                         -- Next buffer
  , ("bp", onGroup W.focusUp')                           -- Prev buffer
  , ("bN", windows W.focusDown)                          -- Next buffer alt
  , ("bP", windows W.focusUp)                            -- Prev buffer alt
  -- , ("bsn", onGroup swapDown')                           -- Swap next buffer
  -- , ("bsp", onGroup swapUp')                             -- Swap prev buffer
  -- , ("bsN", windows W.swapDown)                          -- Swap next buffer alt
  -- , ("bsP", windows W.swapUp)                            -- Swap prev buffer alt
  , ("bS", sendMessage Swap)                             -- Swap groups
  , ("mh", sendMessage $ pullGroup L)                    -- Merge left
  , ("mj", sendMessage $ pullGroup D)                    -- Merge down
  , ("mk", sendMessage $ pullGroup U)                    -- Merge up
  , ("ml", sendMessage $ pullGroup R)                    -- Merge right

  ----------------------------------------------------------------------
  --                           Workspaces                             --
  ----------------------------------------------------------------------
  , ("wb", sendMessage Balance)
  , ("wB", sendMessage Equalize)
  , ("wn", moveTo Next NonEmptyWS)
  , ("wp", moveTo Prev NonEmptyWS)
  , ("wN", nextWS)
  , ("wP", prevWS)
  , ("wf", withFocused (sendMessage . MergeAll)) -- Window focus
  , ("wF", floatPrompt) -- Window float
  , ("wr", resizePrompt) -- Window resize
  , ("wR", sendMessage Rotate) -- Window rotate
  , ("ws", withFocused (sendMessage . UnMerge)) -- Window split
  , ("wS", withFocused (sendMessage . UnMerge) >>
           sendMessage Rotate) -- Window split
  , ("wl", spawn "echo SWITCH >> /tmp/.layout_switch" >> sendMessage NextLayout)
  -- , ("M-S-C-j",  sendMessage $ SplitShift Prev)
  -- , ("M-S-C-k",  sendMessage $ SplitShift Next)
  ]
  ++
  -- Workspace switching and buffer send to workspace
  [ (otherModMasks ++ [key], action tag)
  | (tag, key)  <- zip myWorkspaces "12345"
  , (otherModMasks, action) <-
      [ ("", windows . W.greedyView) , ("bs", windows . W.shift)]
  ]

prefixPrompt :: X ()
prefixPrompt = xmonadPromptC prefixCommands prefixXPConfig

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
