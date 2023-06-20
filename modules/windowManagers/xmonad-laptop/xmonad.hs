{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TupleSections, PatternGuards, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wunused-imports #-}

import XMonad hiding (focus)

import Data.Maybe (isJust)

import Control.Monad

import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Util.WindowProperties
import XMonad.Util.WindowPropertiesRE
import XMonad.Util.Types
import XMonad.Util.WorkspaceCompare
import XMonad.Util.NamedScratchpad
  (customFloating, namedScratchpadAction, namedScratchpadManageHook, NamedScratchpad(..))
import XMonad.Util.Cursor

import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Maximize
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.SubLayouts
import XMonad.Layout.StateFull
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.Simplest
import XMonad.Layout.TwoPane
import XMonad.Layout.LayoutCombinators (JumpToLayout)
import XMonad.Layout.LayoutModifier
import XMonad.Layout.SideBorderDecoration
import XMonad.Layout.NoBorders

import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.DynamicLog hiding (statusBar)
import XMonad.Hooks.StatusBar hiding (withEasySB)
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.RefocusLast

import XMonad.Actions.GroupNavigation
import XMonad.Actions.Plane
import XMonad.Actions.UpdatePointer
import XMonad.Actions.GridSelect

import XMonad.StackSet ( Workspace (..), Stack(..) )
import qualified XMonad.StackSet as W
import qualified XMonad.Core as C
import qualified Data.Map as M
import qualified XMonad.Layout.WindowNavigation as WN
import qualified XMonad.Layout.BoringWindows as BW
import qualified XMonad.Prelude as P
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Hooks.WorkspaceHistory as WH
import qualified XMonad.Layout.Magnifier as Mag

myTerminal = "kitty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth = 2

myNormalBorderColor  = "#ffffff"
myFocusedBorderColor = "#3647d9"

cornerWidth = 400

myModMask = mod4Mask

myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, controlMask)]]

myAdditionalKeys :: [(String, X ())]
myAdditionalKeys =
    [ ("M-x", spawn "rofi -matching fuzzy -show drun -modi drun,run -show -scroll-method 0 -sort -hover-select -me-select-entry '' -me-accept-entry MousePrimary -icon-theme \"crystal-nova\" -show-icons -terminal kitty")
    , ("M-<Escape>", spawn "mate-system-monitor --show-processes-tab")
    , ("M-f", spawn "caja --browser \"/home/cory\"")
    , ("M-e", spawn "emacsclient -c")
    , ("S-M-e", spawn "emacsclient -e '(emacs-everywhere)'")
    , ("M-t", spawn myTerminal)
    , ("M-l", spawn "xscreensaver-command -lock")
    , ("M-s",   sendMessage $ Swap)
    , ("M-r",   sendMessage $ Rotate)
    ----------------------------------------------------------------------
    --                          Brightness                              --
    ----------------------------------------------------------------------
    , ("<XF86MonBrightnessUp>", spawn "light -A 5")
    , ("<XF86MonBrightnessDown>", spawn "light -U 5")

    ----------------------------------------------------------------------
    --                            Audio                                 --
    ----------------------------------------------------------------------
    , ("<XF86AudioLowerVolume>", spawn "vol down")
    , ("<XF86AudioRaiseVolume>", spawn "vol up")
    , ("<XF86AudioMute>", spawn "vol mute")
    , ("<XF86AudioNext>", spawn "strawberry --next")
    , ("<XF86AudioPrev>", spawn "strawberry --previous")
    , ("<XF86AudioPlay>", spawn "strawberry --play-pause")
    , ("<XF86AudioStop>", spawn "strawberry --stop")

    ----------------------------------------------------------------------
      --                          Screenshots                             --
    ----------------------------------------------------------------------
    , ("<Print>", spawn "flameshot gui")
    , ("M-<Print>", spawn "flameshot launcher")
    , ("S-<Print>", spawn "flameshot full -p ~/Pictures/Screenshots")

    ----------------------------------------------------------------------
    --                     Basic Window Management                      --
    ----------------------------------------------------------------------
    -- , ("M1-<Tab>", goToSelected $ myGsconfig myGscolorizer)
    , ("M1-<Tab>", goToSelected def { gs_navigate = myGsnavigation })
    , ("M-<Page_Up>", windows W.focusDown)
    , ("M-<Page_Down>", windows W.focusDown)
    , ("<F2>", windows W.focusDown)
    , ("<F5>", nextMatchWithThis Forward className)
    , ("<F6>", nextMatch History (resource =!? "scratchpad"))
    , ("<F7>", sendMessage Mag.Toggle)
    , ("<F8>", kill)
    , ("<F9>" , nextMatchOrDo Forward (className =? "Emacs") (spawn "emacsclient -c"))
    , ("<F10>", nextMatchOrDo Forward (className =? "firefox" <||> className =? "Chromium-browser") (spawn "firefox"))
    , ("<F11>", nextMatchOrDo Forward (className =? "kitty") (spawn "kitty"))
    , ("<F12>", spawn "layout-switch")
    , ("M-<F12>", runSelectedAction def
                { gs_navigate = myGsnavigation }
                [ ("US Minimak", spawn "setxkbmap us_minimak")
                , ("RU Minimak", spawn "setxkbmap ru_phonetic_minimak")
                , ("US Qwerty",  spawn "setxkbmap us")
                ])
    , ("<Scroll_Lock>", nextMatchOrDo Forward (className =? "discord" <||> className =? "telegram-desktop") (spawn "discord"))
    , ("C-M-<Up>", planeMove (Lines 3) Circular ToUp)
    , ("C-M-<Down>", planeMove (Lines 3) Circular ToDown)
    , ("C-M-<Left>", planeMove (Lines 3) Circular ToLeft)
    , ("C-M-<Right>", planeMove (Lines 3) Circular ToRight)
    , ("M1-M-<Up>", planeShift (Lines 3) Circular ToUp)
    , ("M1-M-<Down>", planeShift (Lines 3) Circular ToDown)
    , ("M1-M-<Left>", planeShift (Lines 3) Circular ToLeft)
    , ("M1-M-<Right>", planeShift (Lines 3) Circular ToRight)
    , ("C-`", namedScratchpadAction myScratchpads "terminal")
    ]

myMouseBindings :: [((ButtonMask, Button), Window -> X ())]
myMouseBindings =
  [ ((mod4Mask, button2), (\_ -> withFocused $ windows . W.sink))
  , ((0, 8), (\_ -> windows W.focusUp))
  , ((0, 9), (\_ -> windows W.focusDown))
  ]

------------------------------------------------------------------------

myGsconfig colorizer = (buildDefaultGSConfig colorizer)
                       { gs_cellheight = 40
                       , gs_cellwidth = 100
                       , gs_navigate = myGsnavigation
                       }

myGscolorizer = colorRangeFromClassName
                     black            -- lowest inactive bg
                     (0x70,0xFF,0x70) -- highest inactive bg
                     black            -- active bg
                     white            -- inactive fg
                     white            -- active fg
  where black = minBound
        white = maxBound

myGsnavigation :: TwoD a (Maybe a)
myGsnavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
 where navKeyMap = M.fromList [
          ((0,xK_Escape), cancel)
         ,((0,xK_Return), select)
         ,((0,xK_slash) , substringSearch myGsnavigation)
         ,((0,xK_Left)  , move (-1,0)   >> myGsnavigation)
         ,((0,xK_j)     , move (-1,0)   >> myGsnavigation)
         ,((0,xK_Right) , move (1,0)    >> myGsnavigation)
         ,((0,xK_l)     , move (1,0)    >> myGsnavigation)
         ,((0,xK_Down)  , move (0,1)    >> myGsnavigation)
         ,((0,xK_e)     , move (0,1)    >> myGsnavigation)
         ,((0,xK_Up)    , move (0,-1)   >> myGsnavigation)
         ,((0,xK_i)     , move (0,-1)  >> myGsnavigation)
         ,((0,xK_space) , setPos (0,0)  >> myGsnavigation)
         ,((0,xK_Tab)   , moveNext      >> myGsnavigation)
         ,((shiftMask,xK_Tab), movePrev >> myGsnavigation)
         ]
       -- The navigation handler ignores unknown key symbols
       navDefaultHandler = const myGsnavigation

------------------------------------------------------------------------

-- Custom 3x3 workspace movement
upWS = planeMove (Lines 3) Circular ToUp
downWS = planeMove (Lines 3) Circular ToDown
leftWS = planeMove (Lines 3) Circular ToLeft
rightWS = planeMove (Lines 3) Circular ToRight
upShiftWS = planeShift (Lines 3) Circular ToUp
downShiftWS = planeShift (Lines 3) Circular ToDown
leftShiftWS = planeShift (Lines 3) Circular ToLeft
rightShiftWS = planeShift (Lines 3) Circular ToRight

------------------------------------------------------------------------

myScratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
                ]
    where
    spawnTerm = myTerminal ++  " --name scratchpad"
    findTerm = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.50
        w = 0.9375
        t = 0.50
        l = 0.0

------------------------------------------------------------------------

-- Spacing
-- top, bottom, right, left
gaps = spacingRaw False (Border 0 0 140 0)
  True (Border 0 0 0 0) True

bsp =
  renamed [Replace "bsp"] $ emptyBSP

myLayout = screenCornerLayoutHook
  . gaps
  . smartBorders
  . refocusLastLayoutHook
  $ Mag.magnifiercz 1.618 (bsp)

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

willFloat :: C.Query Bool
willFloat =
  ask >>= \w -> liftX $
    withDisplay $ \d -> do
      sh <- io $ getWMNormalHints d w
      let isFixedSize = isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
      isTransient <- isJust <$> io (getTransientForHint d w)
      return (isFixedSize || isTransient)

myManageHook = composeAll
  [ stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> doCenterFloat
  , stringProperty "WM_WINDOW_ROLE" =? "pop-up"               --> doCenterFloat
  , title     =? "Save Image"                                 --> doCenterFloat
  , title     =? "Save File"                                  --> doCenterFloat
  , title     =? "Open"                                       --> doCenterFloat
  , title     =? "Open Files"                                 --> doCenterFloat
  , title     =? "emacs-run-launcher"                         --> doCenterFloat
  , resource  =? "nm-applet"                                  --> doCenterFloat
  , resource  =? "stalonetray"                                --> doIgnore
  , resource  =? "desktop_window"                             --> doIgnore
  , resource  =? "kdesktop"                                   --> doIgnore
  , isFullscreen --> doFullFloat
  , fmap not Main.willFloat --> insertPosition Below Newer
  , fmap not Main.willFloat -!> insertPosition Master Newer
  ]  <+> namedScratchpadManageHook myScratchpads

------------------------------------------------------------------------

data ScreenCorner = SCUpperLeft
                  | SCUpperRight
                  | SCLowerLeft
                  | SCLowerRight
                  | SCLeft
                  | SCRight
                  | SCTop
                  | SCBottom
                  deriving (Eq, Ord, Show)

newtype ScreenCornerState = ScreenCornerState (M.Map Window (ScreenCorner, X ()))

instance ExtensionClass ScreenCornerState where
    initialValue = ScreenCornerState M.empty

-- | Add one single @X ()@ action to a screen corner
addScreenCorner :: ScreenCorner -> X () -> X ()
addScreenCorner corner xF = do

    ScreenCornerState m <- XS.get
    (win,xFunc) <- case P.find (\(_,(sc,_)) -> sc == corner) (M.toList m) of

                        Just (w, (_,xF')) -> return (w, xF' >> xF) -- chain X actions
                        Nothing           -> (, xF) <$> createWindowAt corner

    XS.modify $ \(ScreenCornerState m') -> ScreenCornerState $ M.insert win (corner,xFunc) m'

-- | Add a list of @(ScreenCorner, X ())@ tuples
addScreenCorners :: [ (ScreenCorner, X ()) ] -> X ()
addScreenCorners = mapM_ (uncurry addScreenCorner)

-- "Translate" a ScreenCorner to real (x,y) Positions
createWindowAt :: ScreenCorner -> X Window

createWindowAt SCUpperLeft = createEdgeAt 0 0 1 1

createWindowAt SCUpperRight = withDisplay $ \dpy ->
  let w = displayWidth  dpy (defaultScreen dpy) - 1
  in createEdgeAt (P.fi w) 0 1 1

createWindowAt SCLowerLeft = withDisplay $ \dpy ->
  let h = displayHeight dpy (defaultScreen dpy) - 1
  in createEdgeAt 0 (P.fi h) 1 1

createWindowAt SCLowerRight = withDisplay $ \dpy ->
    let w = displayWidth  dpy (defaultScreen dpy) - 1
        h = displayHeight dpy (defaultScreen dpy) - 1
    in createEdgeAt (P.fi w) (P.fi h) 1 1

createWindowAt SCLeft = withDisplay $ \dpy ->
    let h = displayHeight dpy (defaultScreen dpy)
    in createEdgeAt 0 (P.fi cornerWidth) 1 ((P.fi h) - (2 * cornerWidth))

createWindowAt SCRight = withDisplay $ \dpy ->
    let w = displayWidth  dpy (defaultScreen dpy)
        h = displayHeight dpy (defaultScreen dpy)
    in createEdgeAt (P.fi (w - 1)) (P.fi cornerWidth) 1 ((P.fi h) - (2 * cornerWidth))

createWindowAt SCTop = withDisplay $ \dpy ->
    let w = displayWidth  dpy (defaultScreen dpy)
    in createEdgeAt (P.fi cornerWidth) 0 ((P.fi w) - (2 * cornerWidth)) 1

createWindowAt SCBottom = withDisplay $ \dpy ->
    let w = displayWidth  dpy (defaultScreen dpy)
        h = displayHeight dpy (defaultScreen dpy)
    in createEdgeAt (P.fi cornerWidth) (P.fi (h - 1)) ((P.fi w) - (2 * cornerWidth)) 1

createEdgeAt :: XMonad.Position -> XMonad.Position -> XMonad.Dimension -> XMonad.Dimension -> X Window
createEdgeAt x y wi h = withDisplay $ \dpy -> io $ do

    rootw <- rootWindow dpy (defaultScreen dpy)

    let
        visual   = defaultVisualOfScreen $ defaultScreenOfDisplay dpy
        attrmask = cWOverrideRedirect

    w <- allocaSetWindowAttributes $ \attributes -> do

        set_override_redirect attributes True
        createWindow dpy        -- display
                     rootw      -- parent window
                     x          -- x
                     y          -- y
                     wi         -- width
                     h          -- height
                     0          -- border width
                     0          -- depth
                     inputOnly  -- class
                     visual     -- visual
                     attrmask   -- valuemask
                     attributes -- attributes

    -- we only need mouse entry events
    selectInput dpy w enterWindowMask
    mapWindow dpy w
    sync dpy False
    return w

-- | Handle screen corner events
screenCornerEventHook :: Event -> X P.All
screenCornerEventHook CrossingEvent { ev_window = win } = do

    ScreenCornerState m <- XS.get

    case M.lookup win m of
         Just (_, xF) -> xF
         Nothing      -> return ()

    return (P.All True)

screenCornerEventHook _ = return (P.All True)

data ScreenCornerLayout a = ScreenCornerLayout
    deriving ( Read, Show )

instance LayoutModifier ScreenCornerLayout a where
    hook ScreenCornerLayout = withDisplay $ \dpy -> do
        ScreenCornerState m <- XS.get
        io $ mapM_ (raiseWindow dpy) $ M.keys m
    unhook = XMonad.Layout.LayoutModifier.hook

screenCornerLayoutHook :: l a -> ModifiedLayout ScreenCornerLayout l a
screenCornerLayoutHook = ModifiedLayout ScreenCornerLayout

------------------------------------------------------------------------

myEventHook e = do
    screenCornerEventHook e
    refocusLastWhen (return True) e

------------------------------------------------------------------------

myLogHook = historyHook
            >> updatePointer (0.99, 0.90) (0, 0)

------------------------------------------------------------------------

myStartupHook = do
  spawnOnce "emacs --daemon"
  spawnOnce "touchegg --client"
  spawnOnce "conky"
  spawnOnce "nm-applet"
  spawnOnce "blueman-applet"
  spawnOnce "mate-volume-control-status-icon"
  spawnOnce "cbatticon"
  spawnOnce "xscreensaver --no-splash"
  spawnOnce "feh --bg-fill /etc/wallpaper.jpg"
  spawnOnce "sleep 10 && trayer --widthtype pixel --edge right --transparent true --alpha 0 --tint 0xffffff --width 330 --height 50 --distancefrom left --distance 45 --align right --expand false --padding 30 --iconspacing 5"
  spawnOnce "strawberry"
  setWMName "LG3D"
  addScreenCorners [ (SCRight, rightWS >> spawn "xdotool mousemove_relative -- -2238 0")
                   , (SCLeft,  leftWS >> spawn "xdotool mousemove_relative 2238 0")
                   , (SCTop, upWS >> spawn "xdotool mousemove_relative 0 1398")
                   , (SCBottom, downWS >> spawn "xdotool mousemove_relative -- 0 -1398")
                   , (SCUpperLeft, leftWS >> upWS >> spawn "xdotool mousemove 2238 1398")
                   , (SCUpperRight, rightWS >> upWS >> spawn "xdotool mousemove 2 1398")
                   , (SCLowerLeft, leftWS >> downWS >> spawn "xdotool mousemove 2238 2")
                   , (SCLowerRight, rightWS >> downWS >> spawn "xdotool mousemove 2 2")
                   ]
  setDefaultCursor xC_left_ptr

------------------------------------------------------------------------

withEasySB :: LayoutClass l Window
           => StatusBarConfig
           -> (XConfig Layout -> (KeyMask, KeySym))
           -> XConfig l
           -> XConfig l
withEasySB sb k conf = docks . withSB sb $ conf
    { layoutHook = layoutHook conf
    , keys       = (<>) <$> keys' <*> keys conf
    }
  where
    k' conf' = case k conf' of
        (0, 0) ->
            defToggleStrutsKey conf'
        key -> key
    keys' = (`M.singleton` sendMessage ToggleStruts) . k'

statusBar :: LayoutClass l Window
    => String
    -> PP
    -> (XConfig Layout -> (KeyMask, KeySym))
    -> XConfig l
    -> IO (XConfig l)
statusBar cmd pp k conf= do
  sb <- statusBarPipe cmd (pure pp)
  return $ withEasySB sb k conf

------------------------------------------------------------------------

bar = "xmobar /etc/xmobar/xmobarrc"

ppWorkspaces = xmobarPP { ppCurrent = wrap "<icon=/etc/xmobar/" ".xpm/>"
                        , ppHidden = const ""
                        , ppHiddenNoWindows = const ""
                        , ppVisible = const ""
                        , ppTitle = const ""
                        , ppOrder = \(ws:_:_:_) -> [ws]
                        }

-- Key binding to toggle the gap from the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

------------------------------------------------------------------------

main = xmonad
  . ewmhFullscreen
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

  -- hooks, layouts
  layoutHook         = myLayout,
  manageHook         = myManageHook,
  handleEventHook    = myEventHook,
  logHook            = myLogHook,
  startupHook        = myStartupHook
  } `additionalKeysP` myAdditionalKeys `additionalMouseBindings` myMouseBindings
