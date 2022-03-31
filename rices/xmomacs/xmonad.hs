{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
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
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.SubLayouts
import XMonad.Layout.StateFull
import XMonad.Layout.Renamed (Rename (Replace), renamed)

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

myTerminal = "urxvtc -icon $HOME/.icons/icons/48x48/terminal.png"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth = 0

myNormalBorderColor  = "#ffffea"
myFocusedBorderColor = "#3d3c3d"

myModMask       = mod4Mask

myWorkspaces = [ "dev", "web", "com", "mus", "etc" ]

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
    , ("<F5>", spawn "amixer -q set Master unmute 2%-")
    , ("<F6>", spawn "amixer -q set Master unmute 2%+")
    , ("<F7>", spawn "amixer -q set Master toggle")
    , ("<F10>", spawn "audacious --fwd")
    , ("<F9>", spawn "audacious --rew")
    , ("<F8>", spawn "audacious --play-pause")
    -- Brightness
    , ("<XF86MonBrightnessUp>", spawn "xbrightness +5000")
    , ("<XF86MonBrightnessDown>", spawn "xbrightness -5000")
    -- Keyboard Layout
    , ("M-C-<Space>", spawn "/home/cory/manual_installs/layout_switch.sh")
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
menuButton' = [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]

menuButton :: [[Bool]]
menuButton = convertToBool menuButton'

miniButton' :: [[Int]]
miniButton' = [[0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0],
               [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
               [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0]]

miniButton :: [[Bool]]
miniButton = convertToBool miniButton'

maxiButton' :: [[Int]]
maxiButton' = [[0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0],
               [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],
               [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
               [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
               [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
               [0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0]]

maxiButton :: [[Bool]]
maxiButton = convertToBool maxiButton'

closeButton' :: [[Int]]
closeButton' = [[0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0],
                [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
                [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
                [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
                [0,1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,0],
                [0,1,1,1,1,0,0,0,1,1,1,1,0,0,0,1,1,1,1,0],
                [0,1,1,1,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,0],
                [1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1],
                [1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1],
                [0,1,1,1,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,0],
                [0,1,1,1,1,0,0,0,1,1,1,1,0,0,0,1,1,1,1,0],
                [0,1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,0],
                [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0],
                [0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],
                [0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
                [0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0]]

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
            fi distFromRight <= maximizeButtonOffset + buttonSize = focus mainw >> sendMessage (Toggle NBFULL) >> return True
          | fi distFromRight >= minimizeButtonOffset &&
            fi distFromRight <= minimizeButtonOffset + buttonSize = focus mainw >> minimizeWindow mainw >> return True
          | otherwise = return False
    action

defaultThemeWithImageButtons :: Theme
defaultThemeWithImageButtons =
  def { fontName = "xft:Iosevka Nerd Font:size=12"
      , inactiveBorderColor = "#eaeaea"
      , inactiveColor = "#3d3c3d"
      , inactiveTextColor = "#eaeaea"
      , inactiveBorderWidth = 0
      , activeBorderColor = "#7eb7e1"
      , activeColor = "#3d3c3d"
      , activeTextColor = "#7eb7e1"
      , activeBorderWidth = 0
      , urgentBorderColor = "#000000"
      , urgentColor = "#880000"
      , urgentTextColor = "#000000"
      , urgentBorderWidth = 0
      , decoHeight = 50
      , windowTitleIcons = [ (menuButton, CenterLeft 20),
                             (closeButton, CenterRight 20),
                             (maxiButton, CenterRight 60),
                             (miniButton, CenterRight 100) ]
      }

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

-- for the future: want to spawn window within currently selected group,
-- and then if you want it to be separate unmerge it from the group.
-- the new window will already be selected so it will be easy to umerge it

-- Spacing
-- top, bottom, right, left
-- bigGaps = spacingRaw False (Border 200 70 200 200)
--   True (Border 0 0 0 0) True
-- threeGapsSingle = spacingRaw False (Border 200 70 1060 1060)
--   True (Border 0 0 0 0) True
-- threeGapsDouble = spacingRaw False (Border 200 70 1060 200)
--   True (Border 0 0 0 0) True
-- threeGaps = spacingRaw False (Border 200 70 200 200)
--   True (Border 0 0 0 0) True
bigGaps = spacingRaw False (Border 200 115 200 175)
  True (Border 0 25 0 25) True
threeGapsSingle = spacingRaw False (Border 200 140 1060 1060)
  True (Border 0 0 0 0) True
threeGapsDouble = spacingRaw False (Border 200 140 1060 200)
  True (Border 0 0 0 0) True
threeGaps = spacingRaw False (Border 200 140 200 200)
  True (Border 0 0 0 0) True

emacs =
  renamed [Replace "bsp"] $
  (subLayout [] StateFull . bigGaps $ emptyBSP)

full =
  renamed [Replace "full"] $
  (bigGaps $ StateFull)

threeCol =
  renamed [Replace "threeCol"] $
  (subLayout [] StateFull $ (ifMax 2 (ifMax 1
      (threeGapsSingle $ Full)
      (threeGapsDouble $ reflectHoriz $ (ThreeColMid 1 (3/100) (2/3))))
    (threeGaps $ ThreeColMid 1 (3/100) (1/2))))

myLayout = avoidStruts
         . (WN.configurableNavigation WN.noNavigateBorders)
         . smartBorders
         . fullScreenToggle
         . minimize
         . BW.boringWindows
         . windowDeco
         . draggingVisualizer
         $ emacs ||| full ||| threeCol
  where
     windowDeco = windowSwitcherDecorationWithImageButtons
                  shrinkText defaultThemeWithImageButtons
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
    , className =? "io.github.celluloid_player.Celluloid" --> mediaFloat
    , className =? "gwenview"                             --> mediaFloat
    , className =? "Sxiv"                                 --> mediaFloat
    , className =? "Orage"                                --> doCenterFloat
    , className =? "Galculator"                           --> calculatorFloat
    , className =? "Firefox" <&&> resource =? "Toolkit"   --> myRectFloat
    , stringProperty "WM_WINDOW_ROLE"
      =? "GtkFileChooserDialog"                           --> myRectFloat
    , stringProperty "WM_WINDOW_ROLE" =? "pop-up"         --> myRectFloat
    , stringProperty "WM_WINDOW_ROLE" =!? "gimp-image-window-1"
      <&&> className =? "Gimp"                            --> doCenterFloat
    , stringProperty "WM_WINDOW_ROLE" =!? "MainWindow#1"
      <&&> className =? "krita"                           --> doCenterFloat
    , isDialog                                            --> myRectFloat
    , isInProperty "_NET_WM_WINDOW_TYPE"
      "_NET_WM_WINDOW_TYPE_SPLASH"                        --> myRectFloat
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
  { ppCurrent = xmobarColor "#004488" ""
    . wrap "<fc=#880000>*</fc>" "<fc=#880000>*</fc>"
  , ppHidden = xmobarColor "#000000" "" . wrap " " " " . clickable
  , ppHiddenNoWindows = xmobarColor "#b7b7b7" "" . wrap " " " " . clickable
  , ppVisible = xmobarColor "#000000" "" . wrap " " " " . clickable
  , ppUrgent = xmobarColor "#880000" ""
    . wrap "<fc=#880000>*</fc>" "<fc=#880000>*</fc>" . clickable
  --, ppTitle = xmobarColor "#0f0f0f" "" . shorten 40 . wrap "<fn=2>" "</fn>"
  , ppTitle = xmobarColor "#0f0f0f" "" . wrap "<fn=2>" "</fn>"
  , ppOrder = \(ws:_:t:_) -> [ws,t]
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

launcherXPConfig = def { font               = "xft:Iosevka Nerd Font:size=12"
                       , bgColor             = "#d7d7d7"
                       , fgColor             = "#880000"
                       , bgHLight            = "#004488"
                       , fgHLight            = "#d7d7d7"
                       , borderColor         = "#880000"
                       , promptBorderWidth   = 2
                       , position            = CenteredAt (71 % 72) (1 % 2)
                       , alwaysHighlight     = True
                       , height              = 60
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
              { fgHLight            = "#770077"
              , borderColor         = "#770077"
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
               { fgHLight            = "#663311"
               , borderColor         = "#663311"
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

prefixXPConfig = def { font = "xft:Iosevka Nerd Font:size=12"
                     , bgColor = "#d7d7d7"
                     , fgColor = "#005500"
                     , bgHLight = "#004488"
                     , fgHLight = "#d7d7d7"
                     , borderColor = "#005500"
                     , promptBorderWidth = 2
                     , position = CenteredAt (71 % 72) (1 % 2)
                     , alwaysHighlight = False
                     , height = 60
                     , maxComplRows = Just 14
                     , historySize = 256
                     , historyFilter = id
                     , promptKeymap = prefixXPKeymap
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
  , ("bs", sendMessage Swap)                             -- Swap groups
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
  , ("wlb", sendMessage $ JumpToLayout "bsp")
  , ("wlf", sendMessage $ JumpToLayout "full")
  , ("wlt", sendMessage $ JumpToLayout "threeCol")
  -- , ("M-S-C-j",  sendMessage $ SplitShift Prev)
  -- , ("M-S-C-k",  sendMessage $ SplitShift Next)
  ]
  ++
  -- Workspace switching and buffer send to workspace
  [ (otherModMasks ++ [key], action tag)
  | (tag, key)  <- zip myWorkspaces "12345"
  , (otherModMasks, action) <-
      [ ("", windows . W.greedyView) , ("bS", windows . W.shift)]
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
