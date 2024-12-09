;;; Nushell
(use-package nushell-mode
  :hook (nushell-mode . (lambda () (setq-local indent-line-function #'ignore))))

;; (use-package nushell-ts-mode)

;;; Haskell
(use-package haskell-mode
  :hook (haskell-mode . haskell-indentation-mode))

;;; C#

(use-package csharp-mode
  ;; :config
  ;; (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
  )

;; (use-package omnisharp)

;; (use-package dotnet)

(use-package sharper
  :demand t
  :bind
  ("C-c n" . sharper-main-transient))

;;; F#
(use-package fsharp-mode)

;; (use-package eglot-fsharp
;;   :hook (fsharp-mode . cory/eglot-ensure))

;;; Powershell
(use-package powershell)

;; prettify symbols
(defun fsharp-enable-prettify-symbols ()
  (let ((alist '(("->" . #x2192)
                 ("<-" . #x2190)
                 ("|>" . #x22b3)
                 ("<|" . #x22b2))))
    (setq-local prettify-symbols-alist alist)))

(add-hook 'fsharp-mode-hook
          (lambda ()
            (fsharp-enable-prettify-symbols)))

;;; Fvwm
(use-package fvwm-mode
  :hook
  (fvwm-mode . cory/fvwm-mode-init)
  :init
  (defun cory/fvwm-mode-init ()
    (setq-local completion-at-point-functions
	        (list (cape-capf-super #'cory/fvwm-mode-capf #'cape-dabbrev) t)))
  :config
  (defvar cory/fvwm-mode-words
    '("Action" "Active" "ActiveColorset" "ActiveDown" "ActiveFore" "ActiveForeOff"
      "ActivePlacement" "ActivePlacementIgnoresStartsOnPage" "ActiveUp" "All" "AllDesks"
      "AllowRestack" "AllPages" "Alphabetic" "Anim" "Animated" "Animation" "AnimationOff"
      "AutomaticHotkeys" "AutomaticHotkeysOff" "AdjustedPixmap" "BGradient" "Back" "BackColor"
      "Background" "BackingStore" "BackingStoreOff" "BalloonColorset" "bg" "Balloons"
      "BalloonFont" "BalloonYOffset" "BalloonBorderWidth" "BorderColorset" "Borders"
      "BorderWidth" "Bottom" "BoundaryWidth" "Buffer" "Button" "Button0" "Button1" "Button2"
      "Button3" "Button4" "Button5" "Button6" "Button7" "Button8" "Button9" "ButtonGeometry"
      "CGradient" "CaptureHonorsStorsOnPage" "CoptureIgnoresStartsOnPage" "CascadePlacement"
      "Centered" "CirculateHit" "CirculateHitIcon" "CirculateHitShaded" "CirculateSkip"
      "CirculateSkipIcon" "CirculateSkipShaded" "Clear" "ClickToFocus"
      "ClickToFocusPassesClick" "ClickToFocusPassesClickOff" "ClickToFocusRaises"
      "ClickToFocusRaisesOff" "Color" "Colorset" "Context" "Columns" "CurrentDesk"
      "CurrentPage" "CurrentPageAnyDesk" "DrawMotion" "DGradient" "DecorateTransient"
      "Default" "Delay" "DepressableBorder" "Desk" "DontLowerTransient" "DontRaiseTransient"
      "DontShowName" "DontStackTransient" "DontStackTransientParent" "DoubleClick"
      "DoubleClickTime" "Down" "DrawIcons" "DumbPlacement" "DynamicMenu"
      "DynamicPopDownAction" "DynamicPopupAction" "EdgeMoveDelay" "EdgeMoveResistance" "East"
      "Expect" "Effect" "FVWM" "FirmBorder" "Fixed" "FixedPosition" "FixedPPosition"
      "FixedSize" "Flat" "FlickeringMoveWorkaround" "FlickeringQtDialogsWorkaround"
      "FocusColorset" "FocusButton" "FocusFollowsMouse" "FocusStyle" "FollowsFocus"
      "FollowsMouse" "Fore" "Font" "ForeColor" "ForeGround" "Format" "Frame" "Function" "Fvwm"
      "FvwmBorder" "FeedBack" "fg" "fgsh" "fgAlpha" "GNOMEIgnoreHints" "GNOMEUseHints"
      "Geometry" "GrabFocus" "GrabFocusOff" "GrabFocusTransient" "GrabFocusTransientOff"
      "Greyed" "GreyedColorset" "HGradient" "Handles" "HandleWidth" "Height" "HiddenHandles"
      "Hilight3DOff" "Hilight" "Hilight3DThick" "Hilight3DThickness" "Hilight3dThin"
      "HilightBack" "HilightBackOff" "HilightBorderColorset" "HilightColorset" "HilightFore"
      "HintOverride" "HoldSubmenus" "HilightIconTitleColorset" "hi" "Icon" "IconAlpha"
      "IconBox" "IconFill" "IconFont" "IconGrid" "IconOverride" "IconSize" "IconTitle"
      "Iconic" "IconifyWindowGroups" "IconifyWindowGroupsOff" "Icons" "IgnoreRestack"
      "Inactive" "InActive" "IndexedWindowName" "Init" "InitialMapCommand" "Interior" "Item"
      "ItemFormat" "Iterations" "IconTitleColorset" "IconTitleFormat" "IconTitleRelief"
      "IndexedIconName" "IconBackgroundPadding" "IconTint" "KeepWindowGroupsOnDesk" "Last"
      "Layer" "Left" "LeftJustified" "LeftJustify" "Lenience" "LowerTransient" "LeftOfText"
      "Match" "MWM" "MWMBorder" "MWMDecor" "MWMDecorMax" "MWMDecorMenu" "MWMDecorMin"
      "MWMFunctions" "ManagerGeometry" "ManualPlacement" "ManualPlacementHonorsStartsOnPage"
      "ManualPlacementIgnoresStartsOnPage" "MaxWindowSize" "Maximized" "Menu" "MenuColorset"
      "MenuFace" "MiniIcons" "MinOverlapPercentPlacement" "MinOverlapPlacement"
      "MinOverlapPlacementPenalties" "MinOverlapPercentPlacementPenalties" "MiniIcon"
      "MixedVisualWorkaround" "ModalityIsEvil" "Mouse" "MouseFocus" "MouseFocusClickRaises"
      "MouseFocusClickRaisesOff" "Move" "Mwm" "MwmBorder" "MwmButtons" "MwmDecor"
      "MwmFunctions" "MultiPixmap" "NakedTransient" "Never" "NeverFocus" "NoActiveIconOverride"
      "NoBorder" "NoButton" "NoBoundaryWidth" "NoButton" "NoDecorHint" "NoDeskSort"
      "NoFuncHint" "NoGeometry" "NoGeometryWithInfo" "NoHandles" "NoHotkeys" "NoIcon"
      "NoIconAction" "NoIconOverride" "NoIconPosition" "NoIconTitle" "NoIcons" "NoInset"
      "NoLenience" "NoMatch" "NoNormal" "NoOLDecor" "NoOnBottom" "NoOnTop" "NoOverride"
      "NoPPosition" "NoResizeOverride" "NoSticky" "NoShape" "NoTitle" "NoTransientPPosition"
      "NoTransientUSPosition" "NoUSPosition" "NoWarp" "Normal" "North" "Northeast" "Northwest"
      "NotAlphabetic" "OLDecor" "OnBottom" "OnTop" "Once" "OnlyIcons" "OnlyNormal"
      "OnlyOnBottom" "OnlyOnTop" "OnlySkipList" "OnlySticky" "Opacity" "Padding" "Panel"
      "ParentalRelativity" "Periodic" "Pixmap" "PlainButton" "PopdownDelayed" "PopdownDelay"
      "PopupDelay" "PopupAsRootMenu" "PopupAsSubmenu" "PopdownImmediately" "PopupDelayed"
      "PopupImmediately" "PopupOffset" "PositionPlacement" "Quiet" "RGradient"
      "RaiseOverNativeWindows" "RaiseOverUnmanaged" "RaiseTransient" "Raised" "Read"
      "RecaptureHonorsStartsOnPage" "RecaptureIgnoresStartsOnPage" "Rectangle"
      "ReliefThickness" "RemoveSubmenus" "Reset" "Resize" "ResizeHintOverride" "ResizeOpaque"
      "ResizeOutline" "Resolution" "Reverse" "ReverseOrder" "Right" "RightJustified" "Root"
      "RootTransparent" "Rows" "RightTitleRotatedCCW" "SGradient" "SameType" "SaveUnder"
      "SaveUnderDiff" "ScatterWindowGroups" "Screen" "SelectButton" "SelectInPlace"
      "SelectOnRelease" "SelectWarp" "SeparatorsLong" "SeparatorsShort" "ShowCurrentDesk"
      "ShowMapping" "SideColor" "SidePic" "Simple" "SkipMapping" "Slippery" "SlipperyIcon"
      "SmallFont" "SloppyFocus" "SmartPlacement" "SnapAttraction" "SnapGrid" "Solid"
      "SolidSeparators" "Sort" "South" "Southeast" "Southwest" "StackTransientParent"
      "StartIconic" "StartNormal" "StartShaded" "StartsAnyWhere" "StartsLowered"
      "StartsOnDesk" "StartsOnPage" "StartsOnPageIgnoresTransients"
      "StartsOnPageIncludesTransients" "StartsOnScreen" "StartsRaised" "State" "StaysOnBottom"
      "StaysOnTop" "StaysPut" "Sticky" "StickyAcrossDesks" "StickyAcrossDesksIcon"
      "StickyAcrossPagesIcon" "StickyIcon" "StickyStippledIconTitle" "StickyStippledTitle"
      "StippledIconTitle" "StippledTitle" "StippledTitleOff" "SubmenusLeft" "SubmenusRight"
      "Sunk" "StrokeWidth" "sh" "This" "TileCascadePlacement" "TileManualPlacement"
      "TiledPixmap" "Timeout" "Tint" "Title" "TitleAtBottom" "TitleColorset" "TitleFont"
      "TitleAtLeft" "TitleAtRight" "TitleAtTop" "TitleUnderlines0" "TitleUnderlines1"
      "TitleUnderlines2" "TitleWarp" "TitleWarpOff" "Top" "Transient" "Translucent"
      "TrianglesRelief" "TrianglesSolid" "Toggle" "Twist" "Up" "UseBorderStyle" "UseDecor"
      "UseIconName" "UseIconPosition" "UsePPosition" "UseSkipList" "UseStack" "UseStyle"
      "UseTitleStyle" "UseTransientPPosition" "UseTransientUSPosition" "UseUSPosition"
      "UseWinList" "UnderText" "VGradient" "VariablePosition" "Vector" "VerticalMargins"
      "VerticalItemSpacing" "VerticalTitleSpacing" "Width" "WIN" "Wait" "Warp" "WarpTitle"
      "West" "Win" "Window" "WindowBorderWidth" "Window3dBorders" "WindowColorsets"
      "WindowListHit" "WindowListSkip" "WindowShadeScrolls" "WindowShadeShrinks"
      "Window3DBorders" "WindowShadeSteps" "Windows" "XineramaRoot" "YGradient"))

  (defun cory/fvwm-mode-capf ()
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (when bounds
	(list (car bounds)
              (cdr bounds)
              cory/fvwm-mode-words
              :exclusive 'no)))))

;;; SQL

(defvar cory/sql-words
  '("ADD " "CONSTRAINT " "ALL " "ALTER " "COLUMN " "TABLE " "AND " "ANY " "AS " "ASC" "BACKUP "
    "DATABASE" "BETWEEN " "CASE " "CHECK " "CREATE " "DATABASE " "INDEX " "OR " "REPLACE "
    "VIEW " "PROCEDURE " "UNIQUE" "VIEW " "DEFAULT " "DELETE " "DESC" "DISTINCT " "DROP "
    "EXEC " "EXISTS " "FOREIGN " "KEY " "FROM " "FULL" "OUTER " "JOIN " "GROUP " "BY " "HAVING
 " "IN " "INNER " "INSERT " "INTO " "SELECT " "IS " "NULL " "NOT" "LEFT " "LIKE " "LIMIT "
    "ORDER " "PRIMARY " "KEY " "RIGHT " "ROWNUM " "TOP " "SET " "TRUNCATE " "UNION" "ALL "
    "UNIQUE " "UPDATE " "VALUES " "VIEW " "WHERE " "ON " "COUNT "))

(defun cory/sql-capf ()
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            cory/sql-words
            :exclusive 'no))))

(add-hook 'sql-mode-hook
	  (lambda ()
	    (setq-local completion-at-point-functions
			(list (cape-capf-super #'cory/sql-capf #'cape-dabbrev) t))))

;;; XML

(with-eval-after-load 'nxml-mode
  (add-hook 'nxml-mode-hook
	    (lambda ()
	      (setq-local completion-at-point-functions
			  (list (cape-capf-super
				 #'rng-completion-at-point
				 #'cape-dabbrev)))))

  (define-key nxml-mode-map (kbd "C-M-s") #'completion-at-point)
  (define-key nxml-mode-map (kbd "C-M-h") #'nxml-forward-element)
  (define-key nxml-mode-map (kbd "C-M-n") nil)
  (define-key nxml-mode-map (kbd "C-M-p") nil)
  (define-key nxml-mode-map (kbd "C-M-t") #'nxml-backward-element)
  (define-key nxml-mode-map (kbd "C-M-a") #'nxml-backward-paragraph)
  (define-key nxml-mode-map (kbd "C-M-e") #'nxml-forward-paragraph)
  (define-key nxml-mode-map (kbd "C-M-i") #'nxml-mark-paragraph)
  (define-key nxml-mode-map (kbd "M-{") nil)
  (define-key nxml-mode-map (kbd "M-}") nil)
  (define-key nxml-mode-map (kbd "M-i") nil))


;;; Perl

(fset 'perl-mode 'cperl-mode)

;;; Lua

(use-package lua-mode)

;;; Markdown

(use-package markdown-mode)

;;; Yaml

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
