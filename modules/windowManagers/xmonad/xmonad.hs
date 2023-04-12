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
import XMonad.Layout.ComboP

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

myTerminal = "kitty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth = 16

myNormalBorderColor  = "#eeeeee"
myFocusedBorderColor = "#e8e8e8"

barWidth = 110

myModMask = mod4Mask

myWorkspaces = [ "1" "2" "3" "4" "5" "6" 7 "8" "9" ]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_5]
        , (f, m) <- [(W.greedyView, 0), (W.shift, controlMask)]]

myAdditionalKeys :: [(String, X ())]
myAdditionalKeys =
    -- Xmonad prompt
    [ ("M-x", spawn "rofi -show drun -modi drun,run --icon-theme \"Tango\" show-icons")
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

-- fixMinimizedWindow :: X ()
-- fixMinimizedWindow =
--   withFocused $ \win ->
--                   let d = 1
--                       rect = Rectangle
--                   in sendMessage (SetGeometry rect)

--                 withDisplay $ \dpy ->
--                        let dw = displayWidth  dpy (defaultScreen dpy) - 1
--                            dh = displayHeight dpy (defaultScreen dpy) - 1
--                            wy = fi (((fi dh) `div` 2) - (barWidth `div` 2))
--                            wwh = ((fi dw) `div` 2)
--                            wht = ((fi dh) `div` 2) - (barWidth `div` 2)
--                            rect = Rectangle 0 wy wwh wht
--                        in sendMessage (SetGeometry rect)

------------------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $ []

------------------------------------------------------------------------


------------------------------------------------------------------------

-- for the future: want to spawn window within currently selected group,
-- and then if you want it to be separate unmerge it from the group.
-- the new window will already be selected so it will be easy to umerge it

-- Spacing
-- top, bottom, right, left
gaps = spacingRaw False (Border 100 74 180 154)
  True (Border 0 26 0 26) True

emacs =
  renamed [Replace "bsp"] $
  (draggingVisualizer . (maximizeWithPadding 0)
   . subLayout [] StateFull . gaps $ emptyBSP)

-- combined =
--   combineTwoP (TwoPane 0.03 0.5) (tabbed shrinkText def) (tabbed shrinkText def) (ClassName "Firefox")

myLayout = avoidStruts
         . (WN.configurableNavigation WN.noNavigateBorders)
         . lessBorders OnlyScreenFloat
         . minimize
         . BW.boringWindows
         $ emacs ||| combined

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

-- myManageHook = composeAll
--     -- [ className =? "Orage"                                --> doCenterFloat
--     [ className =? "Firefox" <&&> resource =? "Toolkit"   --> myRectFloat
--     , stringProperty "WM_WINDOW_ROLE"
--       =? "GtkFileChooserDialog"                           --> myRectFloat
--     , stringProperty "WM_WINDOW_ROLE" =? "pop-up"         --> myRectFloat
--     -- , isDialog                                            --> myRectFloat
--     -- , isInProperty "_NET_WM_WINDOW_TYPE"
--     --   "_NET_WM_WINDOW_TYPE_SPLASH"                        --> myRectFloat
--     , title     =? "Save Image"                           --> myRectFloat
--     , title     =? "Save File"                            --> myRectFloat
--     , title     =? "Open"                                 --> myRectFloat
--     , title     =? "Open Files"                           --> myRectFloat
--     , resource  =? "desktop_window"                       --> doIgnore
--     , resource  =? "kdesktop"                             --> doIgnore
--     , isFullscreen --> doFullFloat
--     , fmap not willFloat --> insertPosition Below Newer
--     , fmap not willFloat -!> insertPosition Master Newer
--     ]
--   where
--     -- xpos, ypos, width, height
--     myRectFloat = doRectFloat (W.RationalRect (1 % 3) (3 % 10) (1 % 3) (2 % 5))
--     helpFloat = doRectFloat (W.RationalRect (7 % 8) (0 % 1) (1 % 8) (1 % 2))

myManageHook = composeAll
               [ isFullscreen --> doFullFloat
               --, fmap not willFloat -!> unfloat
               , isFullscreen -!> unfloat
               , insertPosition Below Newer
               ]
               where
                 unfloat = ask >>= doF . W.sink

willFloat :: C.Query Bool
willFloat =
  ask >>= \w -> liftX $
    withDisplay $ \d -> do
      sh <- io $ getWMNormalHints d w
      let isFixedSize = isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
      isTransient <- isJust <$> io (getTransientForHint d w)
      return (isFixedSize || isTransient)

------------------------------------------------------------------------

myScratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
              , NS "emacs-scratch" spawnEmacsScratch findEmacsScratch manageEmacsScratch
                ]
    where
    role = stringProperty "WM_WINDOW_ROLE"
    spawnTerm = myTerminal ++  " -name scratchpad"
    findTerm = resource =? "scratchpad"
    manageTerm = nonFloating
    findEmacsScratch = title =? "emacs-scratch"
    spawnEmacsScratch = "emacsclient -a='' -nc --frame-parameters='(quote (name . \"emacs-scratch\"))'"
    manageEmacsScratch = nonFloating

------------------------------------------------------------------------

myEventHook = minimizeEventHook

------------------------------------------------------------------------

myLogHook = return ()

------------------------------------------------------------------------

myStartupHook = do
  spawnOnce "emacs --daemon"
  setWMName "LG3D"

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
