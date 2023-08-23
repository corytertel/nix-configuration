;;; Haskell
(use-package haskell-mode
  :hook (haskell-mode . haskell-indentation-mode))

;; F#
(use-package fsharp-mode)

;; (use-package eglot-fsharp
;;   :hook (fsharp-mode . cory/eglot-ensure))

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
  :hook (fvwm-mode . (lambda ()
		       (setq-local completion-at-point-functions
				   (list (cape-super-capf #'cory/fvwm-mode-capf #'cape-dabbrev) t))
		       (subword-mode t)))
  :config
  (defvar cory/fvwm-mode-words
    '("Action" "Active" "ActiveColorset" "ActiveDown" "ActiveFore" "ActiveForeOff"
      "ActivePlacement" "ActivePlacementIgnoresStartsOnPage" "ActiveUp" "All" "AllDesks"
      "AllowRestack" "AllPages" "Alphabetic" "Anim" "Animated" "Animation" "AnimationOff"
      "AutomaticHotkeys" "AutomaticHotkeysOff" "AdjustedPixmap" "BGradient" "Back" "BackColor"
      "Background" "BackingStore" "BackingStoreOff" "BalloonColorset" "bg" "Balloons"
      "BalloonFont" "BalloonYOffset" "BalloonBorderWidth" "BorderColorset" "Borders" "BorderWidth"
      "Bottom" "BoundaryWidth" "Buffer" "Button" "Button0" "Button1" "Button2" "Button3"
      "Button4" "Button5" "Button6" "Button7" "Button8" "Button9" "ButtonGeometry" "CGradient"
      "CaptureHonorsStorsOnPage" "CoptureIgnoresStartsOnPage" "CascadePlacement" "Centered"
      "CirculateHit" "CirculateHitIcon" "CirculateHitShaded" "CirculateSkip" "CirculateSkipIcon"
      "CirculateSkipShaded" "Clear" "ClickToFocus" "ClickToFocusPassesClick"
      "ClickToFocusPassesClickOff" "ClickToFocusRaises" "ClickToFocusRaisesOff" "Color"
      "Colorset" "Context" "Columns" "CurrentDesk" "CurrentPage" "CurrentPageAnyDesk"
      "DrawMotion" "DGradient" "DecorateTransient" "Default" "Delay" "DepressableBorder"
      "Desk" "DontLowerTransient" "DontRaiseTransient" "DontShowName" "DontStackTransient"
      "DontStackTransientParent" "DoubleClick" "DoubleClickTime" "Down" "DrawIcons"
      "DumbPlacement" "DynamicMenu" "DynamicPopDownAction" "DynamicPopupAction" "EdgeMoveDelay"
      "EdgeMoveResistance" "East" "Expect" "Effect" "FVWM" "FirmBorder" "Fixed" "FixedPosition"
      "FixedPPosition" "FixedSize" "Flat" "FlickeringMoveWorkaround" "FlickeringQtDialogsWorkaround"
      "FocusColorset" "FocusButton" "FocusFollowsMouse" "FocusStyle" "FollowsFocus" "FollowsMouse"
      "Fore" "Font" "ForeColor" "ForeGround" "Format" "Frame" "Function" "Fvwm" "FvwmBorder"
      "FeedBack" "fg" "fgsh" "fgAlpha" "GNOMEIgnoreHints" "GNOMEUseHints" "Geometry" "GrabFocus"
      "GrabFocusOff" "GrabFocusTransient" "GrabFocusTransientOff" "Greyed" "GreyedColorset"
      "HGradient" "Handles" "HandleWidth" "Height" "HiddenHandles" "Hilight3DOff" "Hilight"
      "Hilight3DThick" "Hilight3DThickness" "Hilight3dThin" "HilightBack" "HilightBackOff"
      "HilightBorderColorset" "HilightColorset" "HilightFore" "HintOverride" "HoldSubmenus"
      "HilightIconTitleColorset" "hi" "Icon" "IconAlpha" "IconBox" "IconFill" "IconFont"
      "IconGrid" "IconOverride" "IconSize" "IconTitle" "Iconic" "IconifyWindowGroups"
      "IconifyWindowGroupsOff" "Icons" "IgnoreRestack" "Inactive" "InActive" "IndexedWindowName"
      "Init" "InitialMapCommand" "Interior" "Item" "ItemFormat" "Iterations" "IconTitleColorset"
      "IconTitleFormat" "IconTitleRelief" "IndexedIconName" "IconBackgroundPadding" "IconTint"
      "KeepWindowGroupsOnDesk" "Last" "Layer" "Left" "LeftJustified" "LeftJustify" "Lenience"
      "LowerTransient" "LeftOfText" "Match" "MWM" "MWMBorder" "MWMDecor" "MWMDecorMax"
      "MWMDecorMenu" "MWMDecorMin" "MWMFunctions" "ManagerGeometry" "ManualPlacement"
      "ManualPlacementHonorsStartsOnPage" "ManualPlacementIgnoresStartsOnPage" "MaxWindowSize"
      "Maximized" "Menu" "MenuColorset" "MenuFace" "MiniIcons" "MinOverlapPercentPlacement"
      "MinOverlapPlacement" "MinOverlapPlacementPenalties" "MinOverlapPercentPlacementPenalties"
      "MiniIcon" "MixedVisualWorkaround" "ModalityIsEvil" "Mouse" "MouseFocus" "MouseFocusClickRaises"
      "MouseFocusClickRaisesOff" "Move" "Mwm" "MwmBorder" "MwmButtons" "MwmDecor" "MwmFunctions"
      "MultiPixmap" "NakedTransient" "Never" "NeverFocus" "NoActiveIconOverride" "NoBorder" "NoButton"
      "NoBoundaryWidth" "NoButton" "NoDecorHint" "NoDeskSort" "NoFuncHint" "NoGeometry"
      "NoGeometryWithInfo" "NoHandles" "NoHotkeys" "NoIcon" "NoIconAction" "NoIconOverride"
      "NoIconPosition" "NoIconTitle" "NoIcons" "NoInset" "NoLenience" "NoMatch" "NoNormal" "NoOLDecor"
      "NoOnBottom" "NoOnTop" "NoOverride" "NoPPosition" "NoResizeOverride" "NoSticky" "NoShape"
      "NoTitle" "NoTransientPPosition" "NoTransientUSPosition" "NoUSPosition" "NoWarp" "Normal"
      "North" "Northeast" "Northwest" "NotAlphabetic" "OLDecor" "OnBottom" "OnTop" "Once"
      "OnlyIcons" "OnlyNormal" "OnlyOnBottom" "OnlyOnTop" "OnlySkipList" "OnlySticky" "Opacity"
      "Padding" "Panel" "ParentalRelativity" "Periodic" "Pixmap" "PlainButton" "PopdownDelayed"
      "PopdownDelay" "PopupDelay" "PopupAsRootMenu" "PopupAsSubmenu" "PopdownImmediately"
      "PopupDelayed" "PopupImmediately" "PopupOffset" "PositionPlacement" "Quiet" "RGradient"
      "RaiseOverNativeWindows" "RaiseOverUnmanaged" "RaiseTransient" "Raised" "Read"
      "RecaptureHonorsStartsOnPage" "RecaptureIgnoresStartsOnPage" "Rectangle" "ReliefThickness"
      "RemoveSubmenus" "Reset" "Resize" "ResizeHintOverride" "ResizeOpaque" "ResizeOutline"
      "Resolution" "Reverse" "ReverseOrder" "Right" "RightJustified" "Root" "RootTransparent"
      "Rows" "RightTitleRotatedCCW" "SGradient" "SameType" "SaveUnder" "SaveUnderDiff"
      "ScatterWindowGroups" "Screen" "SelectButton" "SelectInPlace" "SelectOnRelease" "SelectWarp"
      "SeparatorsLong" "SeparatorsShort" "ShowCurrentDesk" "ShowMapping" "SideColor" "SidePic"
      "Simple" "SkipMapping" "Slippery" "SlipperyIcon" "SmallFont" "SloppyFocus" "SmartPlacement"
      "SnapAttraction" "SnapGrid" "Solid" "SolidSeparators" "Sort" "South" "Southeast" "Southwest"
      "StackTransientParent" "StartIconic" "StartNormal" "StartShaded" "StartsAnyWhere"
      "StartsLowered" "StartsOnDesk" "StartsOnPage" "StartsOnPageIgnoresTransients"
      "StartsOnPageIncludesTransients" "StartsOnScreen" "StartsRaised" "State" "StaysOnBottom"
      "StaysOnTop" "StaysPut" "Sticky" "StickyAcrossDesks" "StickyAcrossDesksIcon"
      "StickyAcrossPagesIcon" "StickyIcon" "StickyStippledIconTitle" "StickyStippledTitle"
      "StippledIconTitle" "StippledTitle" "StippledTitleOff" "SubmenusLeft" "SubmenusRight" "Sunk"
      "StrokeWidth" "sh" "This" "TileCascadePlacement" "TileManualPlacement" "TiledPixmap" "Timeout"
      "Tint" "Title" "TitleAtBottom" "TitleColorset" "TitleFont" "TitleAtLeft" "TitleAtRight"
      "TitleAtTop" "TitleUnderlines0" "TitleUnderlines1" "TitleUnderlines2" "TitleWarp" "TitleWarpOff"
      "Top" "Transient" "Translucent" "TrianglesRelief" "TrianglesSolid" "Toggle" "Twist" "Up"
      "UseBorderStyle" "UseDecor" "UseIconName" "UseIconPosition" "UsePPosition" "UseSkipList"
      "UseStack" "UseStyle" "UseTitleStyle" "UseTransientPPosition" "UseTransientUSPosition"
      "UseUSPosition" "UseWinList" "UnderText" "VGradient" "VariablePosition" "Vector"
      "VerticalMargins" "VerticalItemSpacing" "VerticalTitleSpacing" "Width" "WIN" "Wait" "Warp"
      "WarpTitle" "West" "Win" "Window" "WindowBorderWidth" "Window3dBorders" "WindowColorsets"
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
    "DATABASE" "BETWEEN " "CASE " "CHECK " "CREATE " "DATABASE " "INDEX " "OR " "REPLACE " "VIEW "
    "PROCEDURE " "UNIQUE" "VIEW " "DEFAULT " "DELETE " "DESC" "DISTINCT " "DROP " "EXEC " "EXISTS "
    "FOREIGN " "KEY " "FROM " "FULL" "OUTER " "JOIN " "GROUP " "BY " "HAVING " "IN " "INNER "
    "INSERT " "INTO " "SELECT " "IS " "NULL " "NOT" "LEFT " "LIKE " "LIMIT " "ORDER " "PRIMARY "
    "KEY " "RIGHT " "ROWNUM " "TOP " "SET " "TRUNCATE " "UNION" "ALL " "UNIQUE " "UPDATE "
    "VALUES " "VIEW " "WHERE " "ON " "COUNT "))

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
			(list (cape-super-capf #'cory/sql-capf #'cape-dabbrev) t))))

;;; XML

(with-eval-after-load 'nxml-mode
  (add-hook 'nxml-mode-hook
	    (lambda ()
	      (setq-local completion-at-point-functions
			  (list (cape-super-capf
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

;;; APL

;; (use-package gnu-apl-mode)

;; TODO
;; - APL function navigation (able to grab from gnu-apl-mode)
;; - APL documentation lookup
;; - Send to repl (send to open dyalog vterm instance)
(use-package dyalog-mode
  :hook
  (dyalog-mode . (lambda () (set-input-method "dyalog-apl-prefix")))
  :bind
  (:map dyalog-mode-map
   ("C-c <C-i>" . cory/display-dyalog-symbols-help))
  :init
  (add-to-list 'auto-mode-alist '("\\.apl.?\\'" . dyalog-mode))
  :config
  (require 'quail)
  (quail-define-package
   "dyalog-apl-prefix" "Dyalog APL prefix" "Dyalog-Input" t
   "" nil t nil nil nil nil nil nil nil nil t)
  (quail-define-rules
   ("``" ?\N{DIAMOND OPERATOR})
   ("`1" ?\N{DIAERESIS})
   ("`2" ?\N{MACRON})
   ("`3" ?\N{LESS-THAN SIGN})
   ("`4" ?\N{LESS-THAN OR EQUAL TO})
   ("`5" ?\N{EQUALS SIGN})
   ("`6" ?\N{GREATER-THAN OR EQUAL TO})
   ("`7" ?\N{GREATER-THAN SIGN})
   ("`8" ?\N{NOT EQUAL TO})
   ("`9" ?\N{LOGICAL OR})
   ("`0" ?\N{LOGICAL AND})
   ("`-" ?\N{MULTIPLICATION SIGN})
   ("`=" ?\N{DIVISION SIGN})
   ("`~" ?\N{APL FUNCTIONAL SYMBOL QUAD DIAMOND})
   ("`!" ?\N{APL FUNCTIONAL SYMBOL I-BEAM})
   ("`@" ?\N{APL FUNCTIONAL SYMBOL DEL TILDE})
   ("`#" ?\N{APL FUNCTIONAL SYMBOL DEL STILE})
   ("`$" ?\N{APL FUNCTIONAL SYMBOL DELTA STILE})
   ("`%" ?\N{APL FUNCTIONAL SYMBOL CIRCLE STILE})
   ("`^" ?\N{APL FUNCTIONAL SYMBOL CIRCLE BACKSLASH})
   ("`&" ?\N{CIRCLED MINUS})
   ("`*" ?\N{APL FUNCTIONAL SYMBOL CIRCLE STAR})
   ("`(" ?\N{APL FUNCTIONAL SYMBOL DOWN CARET TILDE})
   ("`)" ?\N{APL FUNCTIONAL SYMBOL UP CARET TILDE})
   ("`_" ?\N{EXCLAMATION MARK})
   ("`+" ?\N{APL FUNCTIONAL SYMBOL QUAD DIVIDE})
   ("`q" ?\N{QUESTION MARK})
   ("`w" ?\N{APL FUNCTIONAL SYMBOL OMEGA})
   ("`e" ?\N{SMALL ELEMENT OF})
   ("`r" ?\N{APL FUNCTIONAL SYMBOL RHO})
   ("`t" ?\N{TILDE})
   ("`y" ?\N{UPWARDS ARROW})
   ("`u" ?\N{DOWNWARDS ARROW})
   ("`i" ?\N{APL FUNCTIONAL SYMBOL IOTA})
   ("`o" ?\N{WHITE CIRCLE})
   ("`p" ?\N{ASTERISK})
   ("`[" ?\N{LEFTWARDS ARROW})
   ("`]" ?\N{RIGHTWARDS ARROW})
   ("`\\" ?\N{RIGHT TACK})
   ("`E" ?\N{APL FUNCTIONAL SYMBOL EPSILON UNDERBAR})
   ("`T" ?\N{APL FUNCTIONAL SYMBOL TILDE DIAERESIS})
   ("`I" ?\N{APL FUNCTIONAL SYMBOL IOTA UNDERBAR})
   ("`O" ?\N{APL FUNCTIONAL SYMBOL CIRCLE DIAERESIS})
   ("`P" ?\N{APL FUNCTIONAL SYMBOL STAR DIAERESIS})
   ("`{" ?\N{APL FUNCTIONAL SYMBOL QUOTE QUAD})
   ("`}" ?\N{APL FUNCTIONAL SYMBOL ZILDE})
   ("`|" ?\N{LEFT TACK})
   ("`a" ?\N{APL FUNCTIONAL SYMBOL ALPHA})
   ("`s" ?\N{LEFT CEILING})
   ("`d" ?\N{LEFT FLOOR})
   ("`f" ?\N{LOW LINE})
   ("`g" ?\N{NABLA})
   ("`h" ?\N{INCREMENT})
   ("`j" ?\N{RING OPERATOR})
   ("`k" ?\N{APOSTROPHE})
   ("`l" ?\N{APL FUNCTIONAL SYMBOL QUAD})
   ("`;" ?\N{APL FUNCTIONAL SYMBOL DOWN TACK JOT})
   ("`'" ?\N{APL FUNCTIONAL SYMBOL UP TACK JOT})
   ("`J" ?\N{APL FUNCTIONAL SYMBOL JOT DIAERESIS})
   ("`K" ?\N{APL FUNCTIONAL SYMBOL QUAD EQUAL})
   ("`L" ?\N{APL FUNCTIONAL SYMBOL SQUISH QUAD})
   ("`:" ?\N{IDENTICAL TO})
   ("`\"" ?\N{NOT IDENTICAL TO})
   ("`z" ?\N{SUBSET OF})
   ("`x" ?\N{SUPERSET OF})
   ("`c" ?\N{INTERSECTION})
   ("`v" ?\N{UNION})
   ("`b" ?\N{UP TACK})
   ("`n" ?\N{DOWN TACK})
   ("`m" ?\N{VERTICAL LINE})
   ("`," ?\N{APL FUNCTIONAL SYMBOL UP SHOE JOT})
   ("`." ?\N{APL FUNCTIONAL SYMBOL BACKSLASH BAR})
   ("`/" ?\N{APL FUNCTIONAL SYMBOL SLASH BAR})
   ("`Z" ?\N{SUBSET OF OR EQUAL TO})
   ("`<" ?\N{APL FUNCTIONAL SYMBOL COMMA BAR})
   ("`>" ?\N{APL FUNCTIONAL SYMBOL DELTA UNDERBAR})
   ("`?" ?\N{APL FUNCTIONAL SYMBOL QUAD COLON}))
  (defun cory/display-dyalog-symbols-help ()
    (interactive)
    (let ((buffer-name "*Dyalog Symbols*"))
      (with-current-buffer (get-buffer-create buffer-name)
	(insert
	 (concat
	  (propertize "`` " 'face 'font-lock-string-face)
	  (char-to-string ?\N{DIAMOND OPERATOR}) "	"
	  (propertize "`1 " 'face 'font-lock-string-face)
	  (char-to-string ?\N{DIAERESIS}) "	"
	  (propertize "`2 " 'face 'font-lock-string-face)
	  (char-to-string ?\N{MACRON}) "	"
	  (propertize "`3 " 'face 'font-lock-string-face)
	  (char-to-string ?\N{LESS-THAN SIGN}) "	"
	  (propertize "`4 " 'face 'font-lock-string-face)
	  (char-to-string ?\N{LESS-THAN OR EQUAL TO}) "	"
	  (propertize "`5 " 'face 'font-lock-string-face)
	  (char-to-string ?\N{EQUALS SIGN}) "	"
	  (propertize "`6 " 'face 'font-lock-string-face)
	  (char-to-string ?\N{GREATER-THAN OR EQUAL TO}) "	"
	  (propertize "`7 " 'face 'font-lock-string-face)
	  (char-to-string ?\N{GREATER-THAN SIGN}) "	"
	  (propertize "`8 " 'face 'font-lock-string-face)
	  (char-to-string ?\N{NOT EQUAL TO}) "	"
	  (propertize "`9 " 'face 'font-lock-string-face)
	  (char-to-string ?\N{LOGICAL OR}) "	"
	  (propertize "`0 " 'face 'font-lock-string-face)
	  (char-to-string ?\N{LOGICAL AND}) "	"
	  (propertize "`- " 'face 'font-lock-string-face)
	  (char-to-string ?\N{MULTIPLICATION SIGN}) "	"
	  (propertize "`= " 'face 'font-lock-string-face)
	  (char-to-string ?\N{DIVISION SIGN}) "	" "
"
	  (propertize "`~ " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL QUAD DIAMOND}) "	"
	  (propertize "`! " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL I-BEAM}) "	"
	  (propertize "`@ " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL DEL TILDE}) "	"
	  (propertize "`# " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL DEL STILE}) "	"
	  (propertize "`$ " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL DELTA STILE}) "	"
	  (propertize "`% " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL CIRCLE STILE}) "	"
	  (propertize "`^ " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL CIRCLE BACKSLASH}) "	"
	  (propertize "`& " 'face 'font-lock-string-face)
	  (char-to-string ?\N{CIRCLED MINUS}) "	"
	  (propertize "`* " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL CIRCLE STAR}) "	"
	  (propertize "`( " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL DOWN CARET TILDE}) "	"
	  (propertize "`) " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL UP CARET TILDE}) "	"
	  (propertize "`_ " 'face 'font-lock-string-face)
	  (char-to-string ?\N{EXCLAMATION MARK}) "	"
	  (propertize "`+ " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL QUAD DIVIDE}) "	" "
"
	  (propertize "`q " 'face 'font-lock-string-face)
	  (char-to-string ?\N{QUESTION MARK}) "	"
	  (propertize "`w " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL OMEGA}) "	"
	  (propertize "`e " 'face 'font-lock-string-face)
	  (char-to-string ?\N{SMALL ELEMENT OF}) "	"
	  (propertize "`r " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL RHO}) "	"
	  (propertize "`t " 'face 'font-lock-string-face)
	  (char-to-string ?\N{TILDE}) "	"
	  (propertize "`y " 'face 'font-lock-string-face)
	  (char-to-string ?\N{UPWARDS ARROW}) "	"
	  (propertize "`u " 'face 'font-lock-string-face)
	  (char-to-string ?\N{DOWNWARDS ARROW}) "	"
	  (propertize "`i " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL IOTA}) "	"
	  (propertize "`o " 'face 'font-lock-string-face)
	  (char-to-string ?\N{WHITE CIRCLE}) "	"
	  (propertize "`p " 'face 'font-lock-string-face)
	  (char-to-string ?\N{ASTERISK}) "	"
	  (propertize "`[ " 'face 'font-lock-string-face)
	  (char-to-string ?\N{LEFTWARDS ARROW}) "	"
	  (propertize "`] " 'face 'font-lock-string-face)
	  (char-to-string ?\N{RIGHTWARDS ARROW}) "	"
	  (propertize "`\\ " 'face 'font-lock-string-face)
	  (char-to-string ?\N{RIGHT TACK}) "	" "
"
	  (propertize "`E " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL EPSILON UNDERBAR}) "	"
	  (propertize "`T " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL TILDE DIAERESIS}) "	"
	  (propertize "`I " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL IOTA UNDERBAR}) "	"
	  (propertize "`O " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL CIRCLE DIAERESIS}) "	"
	  (propertize "`P " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL STAR DIAERESIS}) "	"
	  (propertize "`{ " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL QUOTE QUAD}) "	"
	  (propertize "`} " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL ZILDE}) "	"
	  (propertize "`| " 'face 'font-lock-string-face)
	  (char-to-string ?\N{LEFT TACK}) "	"
	  (propertize "`a " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL ALPHA}) "	"
	  (propertize "`s " 'face 'font-lock-string-face)
	  (char-to-string ?\N{LEFT CEILING}) "	"
	  (propertize "`d " 'face 'font-lock-string-face)
	  (char-to-string ?\N{LEFT FLOOR}) "	"
	  (propertize "`f " 'face 'font-lock-string-face)
	  (char-to-string ?\N{LOW LINE}) "	"
	  (propertize "`g " 'face 'font-lock-string-face)
	  (char-to-string ?\N{NABLA}) "	" "
"
	  (propertize "`h " 'face 'font-lock-string-face)
	  (char-to-string ?\N{INCREMENT}) "	"
	  (propertize "`j " 'face 'font-lock-string-face)
	  (char-to-string ?\N{RING OPERATOR}) "	"
	  (propertize "`k " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APOSTROPHE}) "	"
	  (propertize "`l " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL QUAD}) "	"
	  (propertize "`; " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL DOWN TACK JOT}) "	"
	  (propertize "`' " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL UP TACK JOT}) "	"
	  (propertize "`J " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL JOT DIAERESIS}) "	"
	  (propertize "`K " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL QUAD EQUAL}) "	"
	  (propertize "`L " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL SQUISH QUAD}) "	"
	  (propertize "`: " 'face 'font-lock-string-face)
	  (char-to-string ?\N{IDENTICAL TO}) "	"
	  (propertize "`\" " 'face 'font-lock-string-face)
	  (char-to-string ?\N{NOT IDENTICAL TO}) "	"
	  (propertize "`z " 'face 'font-lock-string-face)
	  (char-to-string ?\N{SUBSET OF}) "	"
	  (propertize "`x " 'face 'font-lock-string-face)
	  (char-to-string ?\N{SUPERSET OF}) "	" "
"
	  (propertize "`c " 'face 'font-lock-string-face)
	  (char-to-string ?\N{INTERSECTION}) "	"
	  (propertize "`v " 'face 'font-lock-string-face)
	  (char-to-string ?\N{UNION}) "	"
	  (propertize "`b " 'face 'font-lock-string-face)
	  (char-to-string ?\N{UP TACK}) "	"
	  (propertize "`n " 'face 'font-lock-string-face)
	  (char-to-string ?\N{DOWN TACK}) "	"
	  (propertize "`m " 'face 'font-lock-string-face)
	  (char-to-string ?\N{VERTICAL LINE}) "	"
	  (propertize "`, " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL UP SHOE JOT}) "	"
	  (propertize "`. " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL BACKSLASH BAR}) "	"
	  (propertize "`/ " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL SLASH BAR}) "	"
	  (propertize "`Z " 'face 'font-lock-string-face)
	  (char-to-string ?\N{SUBSET OF OR EQUAL TO}) "	"
	  (propertize "`< " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL COMMA BAR}) "	"
	  (propertize "`> " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL DELTA UNDERBAR}) "	"
	  (propertize "`? " 'face 'font-lock-string-face)
	  (char-to-string ?\N{APL FUNCTIONAL SYMBOL QUAD COLON}) "	"))
	(goto-char (point-min))
	(local-set-key (kbd "q") 'kill-buffer-and-window)
	(not-modified)
	(read-only-mode)
	(olivetti-mode t)
	(setq-local cursor-type nil))
      (pop-to-buffer buffer-name '((display-buffer-in-side-window)
				   (side . top)
				   (window-parameters . ((no-other-window . nil)
							 (mode-line-format . none)))
				   (window-height . fit-window-to-buffer))))))
