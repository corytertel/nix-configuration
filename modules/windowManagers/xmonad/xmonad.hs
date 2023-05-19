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

-- myNormalBorderColor  = "#aaaaaa"
myNormalBorderColor  = "#ffffff"
myFocusedBorderColor = "#3647d9"

barWidth = 110

cornerWidth = 150

myModMask = mod4Mask

myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, controlMask)]]

myAdditionalKeys :: [(String, X ())]
myAdditionalKeys =
    -- Xmonad prompt
    [ ("M-<Space>", spawn "rofi -matching fuzzy -show drun -modi drun,run -icon-theme \"crystal-nova\" -show-icons -terminal kitty")
    -- [ ("M-<Space>", spawn "dmenu_run -nb #ffffff -nf #000000 -sb #3647d9 -sf #ffffff -fn \"Liberation Serif\" -b")
    , ("M-<Escape>", spawn "mate-system-monitor --show-processes-tab")
    , ("M-e", spawn "caja --browser \"/home/cory\"")
    , ("M-w", spawn "emacs")
    , ("M-t", spawn myTerminal)
    , ("M-l", spawn "xscreensaver-command -lock")
    ----------------------------------------------------------------------
    --                          Brightness                              --
    ----------------------------------------------------------------------
    , ("<XF86MonBrightnessUp>", spawn "xbrightness +2500")
    , ("<XF86MonBrightnessDown>", spawn "xbrightness -2500")

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
    -- , ("M1-<Tab>", nextMatch History (return True))
    -- , ("M1-<Tab>", goToSelected $ myGsconfig myGscolorizer)
    , ("M1-<Tab>", goToSelected def { gs_navigate = myGsnavigation })
    , ("M-<Next>", windows W.focusDown)
    , ("M-<Prior>", windows W.focusDown)
    , ("<F2>", windows W.focusDown)
    , ("<F5>", nextMatchWithThis Forward className)
    , ("<F6>", nextMatch History (return True))
    -- , ("<F7>", sendMessage $ JumpToLayout "normal")
    -- , ("<F7>", sendMessage NextLayout)
    , ("<F7>", sendMessage Mag.Toggle)
    , ("<F8>", kill)
    , ("<F9>" , nextMatchOrDo Forward (className =? "Emacs") (spawn "emacsclient -c"))
    , ("<F10>", nextMatchOrDo Forward (className =? "firefox" <||> className =? "chromium-browser") (spawn "firefox"))
    , ("<F11>", nextMatchOrDo Forward (className =? "kitty") (spawn "kitty"))
    , ("<F12>", spawn "layout-switch")
    , ("<Scroll_Lock>", nextMatchOrDo Forward (className =? "discord" <||> className =? "telegram-desktop") (spawn "discord"))
    -- , ("M1-m", sendMessage $ JumpToLayout "messages")
    -- , ("C-S-p", sendMessage $ JumpToLayout "context")
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

-- myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $ []

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

data SwapWindow =  SwapWindow        -- ^ Swap window between panes
                 | SwapWindowN Int   -- ^ Swap window between panes in the N-th nested ComboP. @SwapWindowN 0@ equals to SwapWindow
                 deriving (Read, Show)
instance Message SwapWindow

data PartitionWins = PartitionWins  -- ^ Reset the layout and
                                    -- partition all windows into the
                                    -- correct sub-layout.  Useful for
                                    -- when window properties have
                                    -- changed and you want ComboP to
                                    -- update which layout a window
                                    -- belongs to.
                   deriving (Read, Show)
instance Message PartitionWins

data CombineTwoP l l1 l2 a = C2P [a] [a] [a] l (l1 a) (l2 a) PropertyRE
                                deriving (Read, Show)

combineTwoP :: (LayoutClass super(), LayoutClass l1 Window, LayoutClass l2 Window) =>
                super () -> l1 Window -> l2 Window -> PropertyRE -> CombineTwoP (super ()) l1 l2 Window
combineTwoP = C2P [] [] []

instance (LayoutClass l (), LayoutClass l1 Window, LayoutClass l2 Window) =>
    LayoutClass (CombineTwoP (l ()) l1 l2) Window where
    doLayout (C2P f w1 w2 super l1 l2 prop) rinput s =
        let origws = W.integrate s           -- passed in windows
            w1c = origws `P.intersect` w1      -- current windows in the first pane
            w2c = origws `P.intersect` w2      -- current windows in the second pane
            new = origws P.\\ (w1c ++ w2c)     -- new windows
            superstack = Just Stack { focus=(), up=[], down=[()] }
            f' = focus s:P.delete (focus s) f  -- list of focused windows, contains 2 elements at most
        in do
            matching <- hasPropertyRE prop `filterM` new  -- new windows matching predecate
            let w1' = w1c ++ matching                     -- updated first pane windows
                w2' = w2c ++ (new P.\\ matching)            -- updated second pane windows
                s1 = differentiate f' w1'                 -- first pane stack
                s2 = differentiate f' w2'                 -- second pane stack
            ([((),r1),((),r2)], msuper') <- runLayout (Workspace "" super superstack) rinput
            (wrs1, ml1') <- runLayout (Workspace "" l1 s1) r1
            (wrs2, ml2') <- runLayout (Workspace "" l2 s2) r2
            return  (wrs1++wrs2, Just $ C2P f' w1' w2' (P.fromMaybe super msuper')
                (P.fromMaybe l1 ml1') (P.fromMaybe l2 ml2') prop)

    handleMessage us@(C2P f ws1 ws2 super l1 l2 prop) m
        | Just PartitionWins   <- fromMessage m = return . Just $ C2P [] [] [] super l1 l2 prop
        | Just SwapWindow      <- fromMessage m = swap us
        | Just (SwapWindowN 0) <- fromMessage m = swap us
        | Just (SwapWindowN n) <- fromMessage m = forwardToFocused us $ SomeMessage $ SwapWindowN $ n-1

        | Just (WN.MoveWindowToWindow w1 w2) <- fromMessage m,
          w1 `elem` ws1,
          w2 `elem` ws2 = return $ Just $ C2P f (P.delete w1 ws1) (w1:ws2) super l1 l2 prop

        | Just (WN.MoveWindowToWindow w1 w2) <- fromMessage m,
          w1 `elem` ws2,
          w2 `elem` ws1 = return $ Just $ C2P f (w1:ws1) (P.delete w1 ws2) super l1 l2 prop

        | otherwise = do ml1' <- handleMessage l1 m
                         ml2' <- handleMessage l2 m
                         msuper' <- handleMessage super m
                         if isJust msuper' || isJust ml1' || isJust ml2'
                            then return $ Just $ C2P f ws1 ws2
                                                 (P.fromMaybe super msuper')
                                                 (P.fromMaybe l1 ml1')
                                                 (P.fromMaybe l2 ml2') prop
                            else return Nothing

    description (C2P _ _ _ super l1 l2 prop) = "combining " ++ description l1 ++ " and "++
                                description l2 ++ " with " ++ description super ++ " using "++ show prop

-- send focused window to the other pane. Does nothing if we don't
-- own the focused window
swap :: (LayoutClass s a, LayoutClass l1 Window, LayoutClass l2 Window) =>
        CombineTwoP (s a) l1 l2 Window -> X (Maybe (CombineTwoP (s a) l1 l2 Window))
swap (C2P f ws1 ws2 super l1 l2 prop) = do
    mst <- gets (W.stack . W.workspace . W.current . windowset)
    let (ws1', ws2') = case mst of
            Nothing -> (ws1, ws2)
            Just st -> if foc `elem` ws1
                           then (foc `P.delete` ws1, foc:ws2)
                           else if foc `elem` ws2
                               then (foc:ws1, foc `P.delete` ws2)
                               else (ws1, ws2)
                       where foc = W.focus st
    if (ws1,ws2) == (ws1',ws2')
        then return Nothing
        else return $ Just $ C2P f ws1' ws2' super l1 l2 prop


-- forwards the message to the sublayout which contains the focused window
forwardToFocused :: (LayoutClass l1 Window, LayoutClass l2 Window, LayoutClass s a) =>
                    CombineTwoP (s a) l1 l2 Window -> SomeMessage -> X (Maybe (CombineTwoP (s a) l1 l2 Window))
forwardToFocused (C2P f ws1 ws2 super l1 l2 prop) m = do
    ml1 <- forwardIfFocused l1 ws1 m
    ml2 <- forwardIfFocused l2 ws2 m
    ms <- if isJust ml1 || isJust ml2
            then return Nothing
            else handleMessage super m
    if isJust ml1 || isJust ml2 || isJust ms
        then return $ Just $ C2P f ws1 ws2 (P.fromMaybe super ms) (P.fromMaybe l1 ml1) (P.fromMaybe l2 ml2) prop
        else return Nothing

-- forwards message m to layout l if focused window is among w
forwardIfFocused :: (LayoutClass l Window) => l Window -> [Window] -> SomeMessage -> X (Maybe (l Window))
forwardIfFocused l w m = do
    mst <- gets (W.stack . W.workspace . W.current . windowset)
    maybe (return Nothing) send mst where
    send st = if W.focus st `elem` w
                then handleMessage l m
                else return Nothing

-- code from CombineTwo
-- given two sets of zs and xs takes the first z from zs that also belongs to xs
-- and turns xs into a stack with z being current element. Acts as
-- StackSet.differentiate if zs and xs don't intersect
differentiate :: Eq q => [q] -> [q] -> Maybe (Stack q)
differentiate (z:zs) xs | z `elem` xs = Just $ Stack { focus=z
                                                     , up = reverse $ takeWhile (/=z) xs
                                                     , down = tail $ dropWhile (/=z) xs }
                        | otherwise = differentiate zs xs
differentiate [] xs = W.differentiate xs

------------------------------------------------------------------------

-- for the future: want to spawn window within currently selected group,
-- and then if you want it to be separate unmerge it from the group.
-- the new window will already be selected so it will be easy to umerge it

contextProperties =
  (RE
    (Not (Or (Title "\\*Apropos\\*")
           (Or (Title "\\*cider-inspect.*")
             (Or (Title "\\*Chicken Documentation\\*")
               (Or (Title "\\*Currency\\*")
                 (Or (Title "\\*Dictionary.*")
                   (Or (Title "\\*eldoc.*")
                     (Or (Title "\\*Embark Actions\\*")
                       (Or (Title "\\*Embark Collect.*")
                         (Or (Title "\\*Embark Export.*")
                           (Or (Title "\\*Geiser documentation\\*")
                             (Or (Title "\\*Help.*")
                              (Or (Title "\\*helpful.*")
                                (Or (Title "\\*info\\*")
                                  (Or (Title "\\*lingva\\*")
                                    (Or (Title "\\*lsp-help\\*")
                                      (Or (Title "\\*Metahelp\\*")
                                        (Or (Title "\\*Shortdoc .*")
                                          (Or (Title "\\*sly-apropos.*")
                                            (Or (Title "\\*sly-db")
                                              (Or (Title "\\*sly-description\\*")
                                                (Or (Title "\\*sly-inspector.*")
                                                  (Or (Title "\\*sly-xref.*")
                                                    (Or (Title "\\*Synonyms List\\*")
                                                      (Or (Title "\\*wclock\\*")
                                                        (Or (Title "\\*WoMan.*")
                                                          (Or (Title "\\*WorkNut\\*") (Title "\\*xref\\*")))))))))))))))))))))))))))))

messageProperties =
  (RE
   (Not (Or (Title "\\*Agenda Commands\\*")
         (Or (Title "\\*Async-native-compile-log\\*")
          (Or (Title "\\*Async Shell Command\\*")
           (Or (Title "\\*Backtrace\\*")
            (Or (Title "\\*compilation\\*")
             (Or (Title "\\*Compile-Log\\*")
              (Or (Title "\\*Geiser Debug\\*")
               (Or (Title "\\*Messages\\*")
                (Or (Title "\\*Native-compile-Log\\*")
                 (Or (Title "\\*sly-compilation\\*")
                  (Or (Title "\\*sly-error")
                   (Or (Title "\\*Warning\\*")
                    (Or (Title "\\*Warnings\\*")
                     (Or (Title "\\*Embark Export.*")
                      (Or (Title "\\*Occur.*")
                       (Or (Title "\\*grep\\*")
                        (Or (Title "\\*PDF-Occur\\*")
                         (Or (Title "\\*rg.*") (Title "\\*trace-output\\*")))))))))))))))))))))

-- Spacing
-- top, bottom, right, left
gaps = spacingRaw False (Border 0 0 140 0)
  True (Border 0 0 0 0) True

bsp =
  renamed [Replace "bsp"] $ emptyBSP

normal =
  renamed [Replace "normal"] $
  combineTwoP
  Simplest
  emptyBSP
  Simplest
  (RE (Not (Title "\\*Async Shell Command\\*")))

context =
  renamed [Replace "context"] $
  combineTwoP
  (TwoPane 0.03 0.67)
  emptyBSP
  Simplest
  contextProperties

messages =
  renamed [Replace "messages"] $
  combineTwoP
  (Mirror (TwoPane 0.03 0.8))
  emptyBSP
  Simplest
  messageProperties

contextAndMessages =
  renamed [Replace "contextAndMessages"] $
  combineTwoP
  (Mirror (TwoPane 0.03 0.8))
  context
  Simplest
  messageProperties

focused =
  renamed [Replace "focused"] $ Simplest

-- mySDConfig = def { activeColor         = "#c0daff"
--                  , inactiveColor       = "#ffffff"
--                  , urgentColor         = "#ff00ff"
--                  , activeBorderColor   = "#3647d9"
--                  , inactiveBorderColor = "#3647d9"
--                  , urgentBorderColor   = "#3647d9"
--                  , activeBorderWidth   = 1
--                  , inactiveBorderWidth = 1
--                  , urgentBorderWidth   = 1
--                  , activeTextColor     = "#000000"
--                  , inactiveTextColor   = "#000000"
--                  , urgentTextColor     = "#000000"
--                  , fontName            = "xft:Liberation Serif"
--                  , decoWidth           = 2240
--                  , decoHeight          = 30
--                  , windowTitleAddons   = []
--                  , windowTitleIcons    = []
--                  }

myLayout = screenCornerLayoutHook
  . gaps
  . smartBorders
  $ Mag.magnifiercz 1.618 (bsp)
  -- $ monocle ||| bsp

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

-- myManageHook = composeAll
--                [ isFullscreen --> doFullFloat
--                --, fmap not willFloat -!> unfloat
--                , isFullscreen -!> unfloat
--                , insertPosition Below Newer
--                ]
--                where
--                  unfloat = ask >>= doF . W.sink

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
  , resource  =? "stalonetray"                                --> doIgnore
  , resource  =? "desktop_window"                             --> doIgnore
  , resource  =? "kdesktop"                                   --> doIgnore
  , isFullscreen --> doFullFloat
  , fmap not Main.willFloat --> insertPosition Below Newer
  , fmap not Main.willFloat -!> insertPosition Master Newer
  ]  <+> namedScratchpadManageHook myScratchpads

------------------------------------------------------------------------

-- myScratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
--               , NS "emacs-scratch" spawnEmacsScratch findEmacsScratch manageEmacsScratch
--                 ]
--     where
--     role = stringProperty "WM_WINDOW_ROLE"
--     spawnTerm = myTerminal ++  " -name scratchpad"
--     findTerm = resource =? "scratchpad"
--     manageTerm = nonFloating
--     findEmacsScratch = title =? "emacs-scratch"
--     spawnEmacsScratch = "emacsclient -a='' -nc --frame-parameters='(quote (name . \"emacs-scratch\"))'"
--     manageEmacsScratch = nonFloating

------------------------------------------------------------------------

data ScreenCorner = SCUpperLeftH
                  | SCUpperLeftV
                  | SCUpperRightH
                  | SCUpperRightV
                  | SCLowerLeftH
                  | SCLowerLeftV
                  | SCLowerRightH
                  | SCLowerRightV
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

createWindowAt SCUpperLeftH =
  createEdgeAt 0 0 cornerWidth 1

createWindowAt SCUpperLeftV =
  createEdgeAt 0 0 1 cornerWidth

createWindowAt SCUpperRightH = withDisplay $ \dpy ->
    let w = displayWidth  dpy (defaultScreen dpy)
    in createEdgeAt ((P.fi w) - (P.fi cornerWidth)) 0 cornerWidth 1

createWindowAt SCUpperRightV = withDisplay $ \dpy ->
    let w = displayWidth  dpy (defaultScreen dpy)
    in createEdgeAt (P.fi (w - 1)) 0 1 cornerWidth

createWindowAt SCLowerLeftH = withDisplay $ \dpy ->
    let h = displayHeight dpy (defaultScreen dpy)
    in createEdgeAt 0 (P.fi (h - 1)) cornerWidth 1

createWindowAt SCLowerLeftV = withDisplay $ \dpy ->
    let h = displayHeight dpy (defaultScreen dpy)
    in createEdgeAt 0 ((P.fi h) - (P.fi cornerWidth)) 1 cornerWidth

createWindowAt SCLowerRightH = withDisplay $ \dpy ->
    let w = displayWidth  dpy (defaultScreen dpy)
        h = displayHeight dpy (defaultScreen dpy)
    in createEdgeAt ((P.fi w) - (P.fi cornerWidth)) (P.fi (h - 1)) cornerWidth 1

createWindowAt SCLowerRightV = withDisplay $ \dpy ->
    let w = displayWidth  dpy (defaultScreen dpy)
        h = displayHeight dpy (defaultScreen dpy)
    in createEdgeAt (P.fi (w - 1)) ((P.fi h) - (P.fi cornerWidth)) 1 cornerWidth

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
  -- spawnOnce "trayer --width 140 --widthtype pixel --edge top --align right --transparent true --alpha 0 --tint 0xffffff --distance 190 --height 32"
  spawnOnce "sleep 10 && trayer --widthtype pixel --edge right --transparent true --alpha 0 --tint 0xffffff --width 330 --height 50 --distancefrom left --distance 45 --align right --expand false --padding 30 --iconspacing 5"
  setWMName "LG3D"
  addScreenCorners [ (SCRight, rightWS >> spawn "xdotool mousemove_relative -- -2238 0")
                   , (SCLeft,  leftWS >> spawn "xdotool mousemove_relative 2238 0")
                   , (SCTop, upWS >> spawn "xdotool mousemove_relative 0 1398")
                   , (SCBottom, downWS >> spawn "xdotool mousemove_relative -- 0 -1398")
                   , (SCUpperLeftH, leftWS >> upWS >> spawn "xdotool mousemove 2238 1398")
                   , (SCUpperLeftV, leftWS >> upWS >> spawn "xdotool mousemove 2238 1398")
                   , (SCUpperRightH, rightWS >> upWS >> spawn "xdotool mousemove 2 1398")
                   , (SCUpperRightV, rightWS >> upWS >> spawn "xdotool mousemove 2 1398")
                   , (SCLowerLeftH, leftWS >> downWS >> spawn "xdotool mousemove 2238 2")
                   , (SCLowerLeftV, leftWS >> downWS >> spawn "xdotool mousemove 2238 2")
                   , (SCLowerRightH, rightWS >> downWS >> spawn "xdotool mousemove 2 2")
                   , (SCLowerRightV, rightWS >> downWS >> spawn "xdotool mousemove 2 2")
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
  -- . sideBorder mySideBorderConfig
  =<< statusBar bar ppWorkspaces toggleStrutsKey defaults

-- mySideBorderConfig :: SideBorderConfig
-- mySideBorderConfig = def
--   { sbSide          = D
--   , sbActiveColor   = "#c0daff"
--   , sbInactiveColor = "#ffffff"
--   , sbSize          = 30
--   }

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
        -- mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    } `additionalKeysP` myAdditionalKeys
