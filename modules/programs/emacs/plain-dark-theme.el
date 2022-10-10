;;; plain-dark-theme.el --- Light theme without the distractions.

;; Author: Cory Tertel
;;
;; Version: 0.1
;; Keywords: color, theme
;; Package-Requires: ((emacs "24"))

;; Initially created with the help of emacs-theme-generator,
;; and uses the spacemacs theme as a reference.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:
;;; Code:

(defgroup plaindark-theme nil
  "Plaindark-theme options."
  :group 'faces)

(defcustom plaindark-theme-comment-bg nil
  "Use a background for comment lines."
  :type 'boolean
  :group 'plaindark-theme)

(defcustom plaindark-theme-comment-italic t
  "Enable italics for comments and also disable background."
  :type 'boolean
  :group 'plaindark-theme)

(defcustom plaindark-theme-keyword-italic nil
  "Enable italics for keywords."
  :type 'boolean
  :group 'plaindark-theme)

(defcustom plaindark-theme-org-agenda-height nil
  "If non-nil, use varying text heights for agenda items.

Note that if you change this to a non-nil value, you may want to
also adjust the value of `org-agenda-tags-column'.  If that is set
to 'auto, tags may not be properly aligned."
  :type 'boolean
  :group 'plaindark-theme)

(defcustom plaindark-theme-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'plaindark-theme)

(defcustom plaindark-theme-org-bold t
  "Inherit text bold for org headings."
  :type 'boolean
  :group 'plaindark-theme)

(defcustom plaindark-theme-org-priority-bold t
  "Inherit text bold for priority items in agenda view."
  :type 'boolean
  :group 'plaindark-theme)

(defcustom plaindark-theme-org-highlight nil
  "Highlight org headings."
  :type 'boolean
  :group 'plaindark-theme)

(defcustom plaindark-theme-custom-colors nil
  "Specify a list of custom colors."
  :type 'alist
  :group 'plaindark-theme)

(defcustom plaindark-theme-underline-parens t
  "If non-nil, underline matching parens when using `show-paren-mode' or similar."
  :type 'boolean
  :group 'plaindark-theme)

(defun true-color-p ()
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defun create-plaindark-theme (theme-name)
  (let ((class '((class color) (min-colors 89)))
	(act1          "#444444")
        (act2          "#444444")
        (base          "#ffffff")
        (base-dim      "#505050")
        (bg1           "#1a1a1a")
        (bg2           "#1a1a1a")
        (bg3           "#3c3c3c")
        (bg4           "#4d4d4d")
        (bg-alt        "#5e5e5e")
        (border        "#ffffff")
        (cblk          "#ffffff")
        (cblk-bg       "#242424")
        (cblk-ln       "#2b2b2b")
        (cblk-ln-bg    "#242424")
        (cursor        "#ffffff")
        (const         "#ffffff")
        (comment       "#90caf9")
        (comment-light "#90caf9")
        (comment-bg    "#1a1a1a")
        (comp          "#ed8f23")
        (err           "#e60909")
        (func          "#ed8f23")
        (head1         "#ffffff")
        (head1-bg      "#1a1a1a")
        (head2         "#ffffff")
        (head2-bg      "#1a1a1a")
        (head3         "#ffffff")
        (head3-bg      "#1a1a1a")
        (head4         "#ffffff")
        (head4-bg      "#1a1a1a")
        (highlight     "#343434")
        (highlight-dim "#343434")
        (keyword       "#ffffff")
        (lnum          "#707070")
        (mat           "#ffffff")
        (meta          "#ed8f28")
        (str           "#1f8c35")
        (suc           "#1f8c35")
        (ttip          "#ffffff")
        (ttip-sl       "#404040")
        (ttip-bg       "#303030")
        (type          "#ffffff")
        (var           "#ffffff")
        (war           "#e01bd0")

        ;; colors
        (aqua          "#2d9574")
        (aqua-bg       "#1a1a1a")
        (green         "#1f8c35")
	(green-dim     "#1c8231")
        (green-bg      "#1a1a1a")
        (green-bg-s    "#1a1a1a")
        (cyan          "#2d9574")
	(cyan-dim      "#247a5f")
        (red           "#e60909")
        (red-bg        "#1a1a1a")
        (red-bg-s      "#1a1a1a")
        (blue          "#90caf9")
        (blue-dim      "#085696")
        (blue-bg       "#1a1a1a")
        (blue-bg-s     "#1a1a1a")
        (magenta       "#e01bd0")
        (yellow        "#ed8f23")
        (yellow-dim    "#92540c")
        (yellow-bg     "#1a1a1a"))

    (cl-loop for (cvar . val) in plaindark-theme-custom-colors
             do (set cvar val))

    (custom-theme-set-faces
     theme-name

;;;;; basics
     `(cursor ((,class (:background ,cursor))))
     `(custom-button ((,class :background ,bg2 :foreground ,base :box (:line-width 2 :style released-button))))
     `(default ((,class (:background ,bg1 :foreground ,base))))
     `(default-italic ((,class (:italic t))))
     `(error ((,class (:foreground ,err))))
     `(eval-sexp-fu-flash ((,class (:background ,suc :foreground ,bg1))))
     `(eval-sexp-fu-flash-error ((,class (:background ,err :foreground ,bg1))))
     `(font-lock-builtin-face ((,class (:foreground ,keyword))))
     `(font-lock-comment-face ((,class (:foreground ,(if plaindark-theme-comment-italic comment-light comment) :background ,(when plaindark-theme-comment-bg comment-bg) :slant ,(if plaindark-theme-comment-italic 'italic 'normal)))))
     `(font-lock-constant-face ((,class (:foreground ,const))))
     `(font-lock-doc-face ((,class (:foreground ,meta))))
     `(font-lock-function-name-face ((,class (:foreground ,func :inherit bold))))
     `(font-lock-keyword-face ((,class (:inherit bold :foreground ,keyword :slant ,(if plaindark-theme-keyword-italic 'italic 'normal)))))
     `(font-lock-negation-char-face ((,class (:foreground ,const))))
     `(font-lock-preprocessor-face ((,class (:foreground ,func))))
     `(font-lock-reference-face ((,class (:foreground ,const))))
     `(font-lock-string-face ((,class (:foreground ,str))))
     `(font-lock-type-face ((,class (:foreground ,type :inherit bold))))
     `(font-lock-variable-name-face ((,class (:foreground ,var))))
     `(font-lock-warning-face ((,class (:foreground ,war :background ,bg1))))
     `(fringe ((,class (:background ,bg1 :foreground ,base))))
     `(header-line ((,class :background ,bg2)))
     `(highlight ((,class (:foreground ,base :background ,highlight))))
     `(hl-line ((,class (:background ,bg2 :extend t))))
     `(isearch ((,class (:foreground ,bg1 :background ,mat))))
     `(lazy-highlight ((,class (:background ,green-bg-s :weight normal))))
     `(link ((,class (:foreground ,comment :underline t))))
     `(link-visited ((,class (:foreground ,comp :underline t))))
     `(match ((,class (:background ,highlight :foreground ,mat))))
     `(minibuffer-prompt ((,class (:inherit bold :foreground ,keyword))))
     `(page-break-lines ((,class (:foreground ,act2))))
     `(region ((,class (:background ,highlight :extend t))))
     `(secondary-selection ((,class (:background ,bg3))))
     `(shadow ((,class (:foreground ,base-dim))))
     `(success ((,class (:foreground ,suc))))
     `(tooltip ((,class (:background ,ttip-sl :foreground ,base :bold nil :italic nil :underline nil))))
     `(vertical-border ((,class (:foreground ,border))))
     `(warning ((,class (:foreground ,war))))
     `(widget-button-pressed ((,class (:foreground ,green))))

;;;;; ace-window
     `(aw-leading-char-face ((,class (:foreground ,func :weight bold :height 2.0 :box (:line-width 1 :color ,keyword :style released-button)))))

;;;;; ahs
     `(ahs-face ((,class (:background ,highlight))))
     `(ahs-face-unfocused ((,class (:background ,highlight))))
     `(ahs-definition-face ((,class (:background ,highlight))))
     `(ahs-definition-face-unfocused ((,class (:background ,highlight))))
     `(ahs-plugin-whole-buffer-face ((,class (:background ,mat :foreground ,bg1))))
     `(ahs-plugin-default-face ((,class (:background ,highlight))))
     `(ahs-plugin-default-face-unfocused ((,class (:background ,highlight))))

;;;;; anzu-mode
     `(anzu-mode-line ((,class (:foreground ,yellow :inherit bold))))

;;;;; auto-complete
     `(ac-completion-face ((,class (:background ,ttip-bg :foreground ,ttip))))

;;;;; avy
     `(avy-lead-face   ((,class (:background ,green-bg :foreground ,green))))
     `(avy-lead-face-0 ((,class (:background ,green-bg :foreground ,yellow))))
     `(avy-lead-face-1 ((,class (:background ,green-bg :foreground ,magenta))))
     `(avy-lead-face-2 ((,class (:background ,green-bg :foreground ,blue))))

;;;;; calfw
     `(cfw:face-title               ((,class (:foreground ,head1 :height 2.0 :weight bold :inherit variable-pitch))))
     `(cfw:face-header              ((,class (:foreground ,base :weight bold))))
     `(cfw:face-saturday            ((,class (:foreground ,base :weight bold))))
     `(cfw:face-sunday              ((,class (:foreground ,base :weight bold))))
     `(cfw:face-holiday             ((,class (:foreground ,head1 :weight bold))))
     `(cfw:face-grid                ((,class (:foreground ,border))))
     `(cfw:face-default-content     ((,class (:foreground ,green))))
     `(cfw:face-periods             ((,class (:foreground ,cyan))))
     `(cfw:face-day-title           ((,class (:background ,head1-bg))))
     `(cfw:face-default-day         ((,class (:foreground ,base :weight bold))))
     `(cfw:face-annotation          ((,class (:foreground ,aqua))))
     `(cfw:face-disable             ((,class (:foreground ,base-dim))))
     `(cfw:face-today-title         ((,class (:background ,blue :weight bold))))
     `(cfw:face-today               ((,class (:background ,head1-bg :weight bold))))
     `(cfw:face-select              ((,class (:background ,magenta :weight bold))))
     `(cfw:face-toolbar             ((,class (:foreground ,base :background ,bg1))))
     `(cfw:face-toolbar-button-off  ((,class (:foreground ,base :weight bold))))
     `(cfw:face-toolbar-button-on   ((,class (:foreground ,base :weight bold))))

;;;;; centaur-tabs
     `(centaur-tabs-default ((,class (:background ,bg1 :foreground ,bg1))))
     `(centaur-tabs-selected ((,class (:background ,bg1 :foreground ,base :weight bold))))
     `(centaur-tabs-unselected ((,class (:background ,bg2 :foreground ,base-dim :weight light))))
     `(centaur-tabs-selected-modified ((,class (:background ,bg1
						:foreground ,blue :weight bold))))
     `(centaur-tabs-unselected-modified ((,class (:background ,bg2 :weight light
						  :foreground ,blue))))
     `(centaur-tabs-active-bar-face ((,class (:background ,keyword))))
     `(centaur-tabs-modified-marker-selected ((,class (:inherit 'centaur-tabs-selected :foreground,keyword))))
     `(centaur-tabs-modified-marker-unselected ((,class (:inherit 'centaur-tabs-unselected :foreground,keyword))))

;;;;; cider
     `(cider-enlightened ((,class (:background nil :box (:color ,yellow :line-width -1 :style nil) :foreground ,yellow))))
     `(cider-enlightened-local ((,class (:foreground ,yellow))))
     `(cider-instrumented-face ((,class (:background nil :box (:color ,red :line-width -1 :style nil) :foreground ,red))))
     `(cider-result-overlay-face ((,class (:background nil :box (:color ,blue :line-width -1 :style nil) :foreground ,blue))))
     `(cider-test-error-face ((,class (:background ,war :foreground ,bg1))))
     `(cider-test-failure-face ((,class (:background ,err :foreground ,bg1))))
     `(cider-test-success-face ((,class (:background ,suc :foreground ,bg1))))
     `(cider-traced-face ((,class :box (:color ,cyan :line-width -1 :style nil))))

;;;;; company
     `(company-echo-common ((,class (:background ,base :foreground ,bg1))))
     `(company-preview ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(company-preview-common ((,class (:background ,ttip-bg :foreground ,base))))
     `(company-preview-search ((,class (:inherit match))))
     `(company-scrollbar-bg ((,class (:background ,bg2))))
     `(company-scrollbar-fg ((,class (:background ,act2))))
     `(company-template-field ((,class (:inherit region))))
     `(company-tooltip ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(company-tooltip-annotation ((,class (:foreground ,type))))
     `(company-tooltip-common ((,class (:background ,ttip-bg :foreground ,comment :inherit bold))))
     `(company-tooltip-common-selection ((,class (:foreground ,comment :inherit bold))))
     `(company-tooltip-mouse ((,class (:inherit highlight))))
     `(company-tooltip-search ((,class (:inherit match))))
     `(company-tooltip-selection ((,class (:background ,ttip-sl :foreground ,base))))

;;;;; corfu
     `(corfu-default ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(corfu-current ((,class (:background ,ttip-sl :foreground ,base))))
     `(corfu-bar ((,class (:background ,base))))
     `(corfu-border ((,class (:background ,bg2))))
     `(corfu-echo ((,class (:background ,base :foreground ,bg1))))
     `(corfu-annotations ((,class (:foreground ,type))))
     `(corfu-deprecated ((,class (:foreground ,war :strike-through t))))

;;;;; diff
     `(diff-added             ((,class :background nil :foreground ,green :extend t)))
     `(diff-changed           ((,class :background nil :foreground ,blue)))
     `(diff-header            ((,class :background ,cblk-ln-bg :foreground ,func :extend t)))
     `(diff-file-header       ((,class :background ,cblk-ln-bg :foreground ,cblk :extend t)))
     `(diff-indicator-added   ((,class :background nil :foreground ,green :extend t)))
     `(diff-indicator-changed ((,class :background nil :foreground ,blue)))
     `(diff-indicator-removed ((,class :background nil :foreground ,red)))
     `(diff-refine-added      ((,class :background ,green :foreground ,bg1)))
     `(diff-refine-changed    ((,class :background ,blue :foreground ,bg1)))
     `(diff-refine-removed    ((,class :background ,red :foreground ,bg1)))
     `(diff-removed           ((,class :background nil :foreground ,red :extend t)))

;;;;; diff-hl
     `(diff-hl-insert ((,class :background ,green :foreground ,green)))
     `(diff-hl-delete ((,class :background ,red :foreground ,red)))
     `(diff-hl-change ((,class :background ,blue :foreground ,blue)))

;;;;; dired
     `(dired-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
     `(dired-flagged ((,class (:foreground ,red))))
     `(dired-header ((,class (:foreground ,comp :inherit bold))))
     `(dired-ignored ((,class (:inherit shadow))))
     `(dired-mark ((,class (:foreground ,comp :inherit bold))))
     `(dired-marked ((,class (:foreground ,magenta :inherit bold))))
     `(dired-perm-write ((,class (:foreground ,base :underline t))))
     `(dired-symlink ((,class (:foreground ,cyan :background ,bg1 :inherit bold))))
     `(dired-warning ((,class (:foreground ,war))))

;;;;; doom-modeline
     `(doom-modeline-bar ((,class (:background ,keyword))))

;;;;; ediff
     `(ediff-current-diff-A ((,class(:background ,red-bg :foreground ,red :extend t))))
     `(ediff-current-diff-Ancestor ((,class(:background ,aqua-bg :foreground ,aqua :extend t))))
     `(ediff-current-diff-B ((,class(:background ,green-bg :foreground ,green :extend t))))
     `(ediff-current-diff-C ((,class(:background ,blue-bg :foreground ,blue :extend t))))
     `(ediff-even-diff-A ((,class(:background ,bg3 :extend t))))
     `(ediff-even-diff-Ancestor ((,class(:background ,bg3 :extend t))))
     `(ediff-even-diff-B ((,class(:background ,bg3 :extend t))))
     `(ediff-even-diff-C ((,class(:background ,bg3 :extend t))))
     `(ediff-fine-diff-A ((,class(:background ,red :foreground ,bg1 :extend t))))
     `(ediff-fine-diff-Ancestor ((,class(:background nil :inherit bold :extend t))))
     `(ediff-fine-diff-B ((,class(:background ,green :foreground ,bg1))))
     `(ediff-fine-diff-C ((,class(:background ,blue :foreground ,bg1))))
     `(ediff-odd-diff-A ((,class(:background ,bg4 :extend t))))
     `(ediff-odd-diff-Ancestor ((,class(:background ,bg4 :extend t))))
     `(ediff-odd-diff-B ((,class(:background ,bg4 :extend t))))
     `(ediff-odd-diff-C ((,class(:background ,bg4 :extend t))))

;;;;; eglot
     `(eglot-highlight-symbol-face ((,class (:inherit bold :background ,bg3))))

;;;;; ein
     `(ein:cell-input-area((,class (:background ,bg2))))
     `(ein:cell-input-prompt ((,class (:foreground ,suc))))
     `(ein:cell-output-prompt ((,class (:foreground ,err))))
     `(ein:notification-tab-normal ((,class (:foreground ,keyword))))
     `(ein:notification-tab-selected ((,class (:foreground ,suc :inherit bold))))

;;;;; eldoc
     `(eldoc-highlight-function-argument ((,class (:foreground ,mat :inherit bold))))

;;;;; elfeed
     `(elfeed-search-date-face ((,class (:foreground ,head2))))
     `(elfeed-search-feed-face ((,class (:foreground ,blue))))
     `(elfeed-search-tag-face ((,class (:foreground ,func))))
     `(elfeed-search-title-face ((,class (:foreground ,var))))
     `(elfeed-search-unread-title-face ((,class (:foreground ,base))))

;;;;; enh-ruby
     `(enh-ruby-op-face ((,class (:background ,bg1 :foreground ,base))))
     `(enh-ruby-string-delimiter-face ((,class (:foreground ,str))))

;;;;; erc
     `(erc-input-face ((,class (:foreground ,func))))
     `(erc-my-nick-face ((,class (:foreground ,keyword))))
     `(erc-nick-default-face ((,class (:foreground ,keyword))))
     `(erc-nick-prefix-face ((,class (:foreground ,yellow))))
     `(erc-notice-face ((,class (:foreground ,str))))
     `(erc-prompt-face ((,class (:foreground ,mat :inherit bold))))
     `(erc-timestamp-face ((,class (:foreground ,keyword))))

;;;;; eshell
     `(eshell-ls-archive ((,class (:foreground ,red :inherit bold))))
     `(eshell-ls-backup ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-clutter ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-directory ((,class (:foreground ,keyword :inherit bold))))
     `(eshell-ls-executable ((,class (:foreground ,suc :inherit bold))))
     `(eshell-ls-missing ((,class (:inherit font-lock-warning-face))))
     `(eshell-ls-product ((,class (:inherit font-lock-doc-face))))
     `(eshell-ls-special ((,class (:foreground ,yellow :inherit bold))))
     `(eshell-ls-symlink ((,class (:foreground ,cyan :inherit bold))))
     `(eshell-ls-unreadable ((,class (:foreground ,base))))
     `(eshell-prompt ((,class (:foreground ,keyword :inherit bold))))

;;;;; ESS
     `(ess-assignment-face ((,class (:foreground ,type :inherit bold))))
     `(ess-backquoted-face ((,class (:foreground ,var))))
     `(ess-constant-face ((,class (:inherit font-lock-constant-face))))
     `(ess-f-t-face ((,class (:inherit font-lock-constant-face))))
     `(ess-function-call-face ((,class (:foreground ,func))))
     `(ess-keyword-face ((,class (:inherit font-lock-keyword-face))))
     `(ess-matrix-face ((,class (:foreground ,base-dim))))
     `(ess-modifiers-face ((,class (:foreground ,keyword))))
     `(ess-numbers-face ((,class (:inherit font-lock-constant-face))))
     `(ess-operator-face ((,class (:foreground ,var))))
     `(ess-paren-face ((,class (:foreground ,blue))))
     `(ess-r-control-flow-keyword-face ((,class (:foreground ,keyword))))
     `(ess-r-signal-keyword-face ((,class (:foreground ,war))))

;;;;; evil
     `(evil-ex-lazy-highlight ((,class (:background ,mat :foreground ,bg1))))
     `(evil-ex-substitute-matches ((,class (:background ,red-bg :foreground ,red))))
     `(evil-ex-substitute-replacement ((,class (:background ,green-bg :foreground ,green))))

;;;;; evil-goggles
     `(evil-goggles--pulse-face ((,class (:background ,yellow-bg :foreground ,yellow))))
     `(evil-goggles-change-face ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(evil-goggles-commentary-face ((,class (:background ,aqua-bg :foreground ,aqua))))
     `(evil-goggles-delete-face ((,class (:background ,red-bg-s :foreground ,red))))
     `(evil-goggles-fill-and-move-face ((,class (:background ,green-bg-s :foreground ,green))))
     `(evil-goggles-indent-face ((,class (:background ,green-bg-s :foreground ,green))))
     `(evil-goggles-join-face ((,class (:background ,green-bg-s :foreground ,green))))
     `(evil-goggles-nerd-commenter-face ((,class (:background ,aqua-bg :foreground ,aqua))))
     `(evil-goggles-paste-face ((,class (:background ,green-bg-s :foreground ,green))))
     `(evil-goggles-record-macro-face ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(evil-goggles-replace-with-register-face ((,class (:background ,yellow-bg :foreground ,yellow))))
     `(evil-goggles-set-marker-face ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(evil-goggles-shift-face ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(evil-goggles-surround-face ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(evil-goggles-yank-face ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(evil-goggles-undo-redo-add-face ((,class (:background ,green-bg-s :foreground ,green))))
     `(evil-goggles-undo-redo-change-face ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(evil-goggles-undo-redo-remove-face ((,class (:background ,red-bg-s :foreground ,red))))

;;;;; evil-mc
     `(evil-mc-cursor-bar-face ((,class (:foreground ,aqua))))
     `(evil-mc-cursor-default-face ((,class (:background ,aqua :foreground ,bg4))))
     `(evil-mc-cursor-hbar-face ((,class (:foreground ,aqua))))
     `(evil-mc-region-face ((,class (:inherit highlight))))

;;;;; flycheck
     `(flycheck-error
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,err)))
        (,class (:foreground ,base :background ,err :inherit bold :underline t))))
     `(flycheck-error-list-checker-name ((,class (:foreground ,keyword))))
     `(flycheck-fringe-error ((,class (:foreground ,err :inherit bold))))
     `(flycheck-fringe-info ((,class (:foreground ,keyword :inherit bold))))
     `(flycheck-fringe-warning ((,class (:foreground ,war :inherit bold))))
     `(flycheck-info
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,keyword)))
        (,class (:foreground ,base :background ,keyword :inherit bold :underline t))))
     `(flycheck-warning
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,war)))
        (,class (:foreground ,base :background ,war :inherit bold :underline t))))

;;;;; flymake
     `(flymake-error ((,(append '((supports :underline (:style line))) class)
                       (:underline (:style line :color ,err)))
                      (,class (:foreground ,base :background ,err :inherit bold :underline t))))
     `(flymake-note ((,(append '((supports :underline (:style line))) class)
                      (:underline (:style wave :color ,keyword)))
                     (,class (:foreground ,base :background ,keyword :inherit bold :underline t))))
     `(flymake-warning ((,(append '((supports :underline (:style line))) class)
                         (:underline (:style line :color ,war)))
                        (,class (:foreground ,base :background ,war :inherit bold :underline t))))

;;;;; flyspell
     `(flyspell-incorrect ((,(append '((supports :underline (:style line))) class)
                            (:underline (:style wave :color ,war)))
                           (,class (:foreground ,base :background ,war :inherit bold :underline t))))
     `(flyspell-duplicate ((,(append '((supports :underline (:style line))) class)
                            (:underline (:style wave :color ,keyword)))
                           (,class (:foreground ,base :background ,keyword :inherit bold :underline t))))

;;;;; frog-menu
     `(frog-menu-border ((,class (:background ,ttip))))
     `(frog-menu-posframe-background-face ((,class (:background ,ttip-bg))))

;;;;; jabber
     `(jabber-activity-face ((,class (:inherit bold :foreground ,red))))
     `(jabber-activity-personal-face ((,class (:inherit bold :foreground ,blue))))
     `(jabber-chat-error ((,class (:inherit bold :foreground ,red))))
     `(jabber-chat-prompt-foreign ((,class (:inherit bold :foreground ,red))))
     `(jabber-chat-prompt-local ((,class (:inherit bold :foreground ,blue))))
     `(jabber-chat-prompt-system ((,class (:inherit bold :foreground ,green))))
     `(jabber-chat-text-foreign ((,class (:foreground ,base))))
     `(jabber-chat-text-local ((,class (:foreground ,base))))
     `(jabber-rare-time-face ((,class (:foreground ,green))))
     `(jabber-roster-user-away ((,class (:foreground ,yellow))))
     `(jabber-roster-user-chatty ((,class (:inherit bold :foreground ,green))))
     `(jabber-roster-user-dnd ((,class (:foreground ,red))))
     `(jabber-roster-user-error ((,class (:foreground ,err))))
     `(jabber-roster-user-offline ((,class (:foreground ,base))))
     `(jabber-roster-user-online ((,class (:inherit bold :foreground ,green))))
     `(jabber-roster-user-xa ((,class (:foreground ,aqua))))

;;;;; git-gutter-fr
     `(git-gutter-fr:added ((,class (:foreground ,green :inherit bold))))
     `(git-gutter-fr:deleted ((,class (:foreground ,red :inherit bold))))
     `(git-gutter-fr:modified ((,class (:foreground ,blue :inherit bold))))

;;;;; git-timemachine
     `(git-timemachine-minibuffer-detail-face ((,class (:foreground ,blue :inherit bold :background ,blue-bg))))

;;;;; gnus
     `(gnus-emphasis-highlight-words ((,class (:background ,suc :foreground ,bg1))))
     `(gnus-header-content ((,class (:foreground ,keyword))))
     `(gnus-header-from ((,class (:foreground ,var))))
     `(gnus-header-name ((,class (:foreground ,comp))))
     `(gnus-header-subject ((,class (:foreground ,func :inherit bold))))
     `(gnus-summary-cancelled ((,class (:background ,war :foreground ,bg1))))

;;;;; guide-key
     `(guide-key/highlight-command-face ((,class (:foreground ,base))))
     `(guide-key/key-face ((,class (:foreground ,keyword))))
     `(guide-key/prefix-command-face ((,class (:foreground ,keyword :inherit bold))))

;;;;; helm
     `(helm-bookmark-directory ((,class (:inherit helm-ff-directory))))
     `(helm-bookmark-file ((,class (:foreground ,base))))
     `(helm-bookmark-gnus ((,class (:foreground ,comp))))
     `(helm-bookmark-info ((,class (:foreground ,comp))))
     `(helm-bookmark-man ((,class (:foreground ,comp))))
     `(helm-bookmark-w3m ((,class (:foreground ,comp))))
     `(helm-buffer-directory ((,class (:foreground ,base :background ,bg1))))
     `(helm-buffer-file ((,class (:foreground ,base :background ,bg1))))
     `(helm-buffer-not-saved ((,class (:foreground ,comp :background ,bg1))))
     `(helm-buffer-process ((,class (:foreground ,keyword :background ,bg1))))
     `(helm-buffer-saved-out ((,class (:foreground ,base :background ,bg1))))
     `(helm-buffer-size ((,class (:foreground ,base :background ,bg1))))
     `(helm-candidate-number ((,class (:background ,bg1 :foreground ,keyword :inherit bold))))
     `(helm-ff-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
     `(helm-ff-dotted-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
     `(helm-ff-dotted-symlink-directory ((,class (:foreground ,cyan :background ,bg1 :inherit bold))))
     `(helm-ff-executable ((,class (:foreground ,suc :background ,bg1 :weight normal))))
     `(helm-ff-file ((,class (:foreground ,base :background ,bg1 :weight normal))))
     `(helm-ff-invalid-symlink ((,class (:foreground ,red :background ,bg1 :inherit bold))))
     `(helm-ff-prefix ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
     `(helm-ff-symlink ((,class (:foreground ,cyan :background ,bg1 :inherit bold))))
     `(helm-grep-cmd-line ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-file ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-finish ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-lineno ((,class (:foreground ,type :background ,bg1 :inherit bold))))
     `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
     `(helm-header ((,class (:foreground ,base :background ,bg1 :underline nil :box nil))))
     `(helm-header-line-left-margin ((,class (:foreground ,keyword :background ,nil))))
     `(helm-match ((,class (:background ,head1-bg :foreground ,head1))))
     `(helm-match-item ((,class (:background ,head1-bg :foreground ,head1))))
     `(helm-moccur-buffer ((,class (:foreground ,var :background ,bg1))))
     `(helm-selection ((,class (:background ,highlight))))
     `(helm-selection-line ((,class (:background ,bg2))))
     `(helm-separator ((,class (:foreground ,comp :background ,bg1))))
     `(helm-source-header ((,class (:background ,comp :foreground ,bg1 :inherit bold))))
     `(helm-time-zone-current ((,class (:foreground ,keyword :background ,bg1))))
     `(helm-time-zone-home ((,class (:foreground ,comp :background ,bg1))))
     `(helm-visible-mark ((,class (:foreground ,keyword :background ,bg3))))

;;;;; helm-swoop
     `(helm-swoop-target-line-block-face ((,class (:foreground ,base :background ,highlight))))
     `(helm-swoop-target-line-face ((,class (:background ,highlight))))
     `(helm-swoop-target-word-face ((,class (:background ,highlight :foreground ,mat))))

;;;;; highlights
     `(hi-green  ((,class (:foreground ,green :background ,green-bg))))
     `(hi-yellow ((,class (:foreground ,yellow :background ,yellow-bg))))

;;;;; highlight-indentation
     `(highlight-indentation-face ((,class (:background ,comment-bg))))

;;;;; highlight-symbol
     `(highlight-symbol-face ((,class (:background ,bg-alt))))

;;;;; highlight-thing
     `(highlight-thing       ((,class (:background ,bg-alt))))

;;;;; hydra
     `(hydra-face-blue ((,class (:foreground ,blue))))
     `(hydra-face-red ((,class (:foreground ,red))))

;;;;; ido
     `(ido-first-match ((,class (:foreground ,comp :inherit bold))))
     `(ido-only-match ((,class (:foreground ,mat :inherit bold))))
     `(ido-subdir ((,class (:foreground ,keyword))))
     `(ido-vertical-match-face ((,class (:foreground ,comp :underline nil))))

;;;;; info
     `(info-header-xref ((,class (:foreground ,func :underline t))))
     `(info-menu ((,class (:foreground ,suc))))
     `(info-node ((,class (:foreground ,func :inherit bold))))
     `(info-quoted-name ((,class (:foreground ,keyword))))
     `(info-reference-item ((,class (:background nil :underline t :inherit bold))))
     `(info-string ((,class (:foreground ,str))))
     `(info-title-1 ((,class (:height 1.4 :inherit bold))))
     `(info-title-2 ((,class (:height 1.3 :inherit bold))))
     `(info-title-3 ((,class (:height 1.3))))
     `(info-title-4 ((,class (:height 1.2))))

;;;;; ivy
     `(ivy-current-match ((,class (:background ,highlight :inherit bold :extend t))))
     `(ivy-minibuffer-match-face-1 ((,class (:inherit bold))))
     `(ivy-minibuffer-match-face-2 ((,class (:foreground ,head1 :underline t))))
     `(ivy-minibuffer-match-face-3 ((,class (:foreground ,head4 :underline t))))
     `(ivy-minibuffer-match-face-4 ((,class (:foreground ,head3 :underline t))))
     `(ivy-remote ((,class (:foreground ,cyan))))

;;;;; ivy-posframe
     `(ivy-posframe ((,class (:background ,bg3))))

;;;;; latex
     `(font-latex-bold-face ((,class (:foreground ,comp))))
     `(font-latex-italic-face ((,class (:foreground ,keyword :italic t))))
     `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
     `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
     `(font-latex-sectioning-0-face ((,class (:inherit bold :foreground ,head3 :height ,(if plaindark-theme-org-height 1.3 1.0) :background ,(when plaindark-theme-org-highlight head3-bg)))))
     `(font-latex-sectioning-1-face ((,class (:inherit bold :foreground ,head4 :height ,(if plaindark-theme-org-height 1.3 1.0) :background ,(when plaindark-theme-org-highlight head4-bg)))))
     `(font-latex-sectioning-2-face ((,class (:inherit bold :foreground ,head1 :height ,(if plaindark-theme-org-height 1.3 1.0) :background ,(when plaindark-theme-org-highlight head1-bg)))))
     `(font-latex-sectioning-3-face ((,class (:inherit bold :foreground ,head2 :height ,(if plaindark-theme-org-height 1.2 1.0) :background ,(when plaindark-theme-org-highlight head2-bg)))))
     `(font-latex-sectioning-4-face ((,class (:bold nil :foreground ,head3 :height ,(if plaindark-theme-org-height 1.1 1.0) :background ,(when plaindark-theme-org-highlight head3-bg)))))
     `(font-latex-sectioning-5-face ((,class (:bold nil :foreground ,head4 :background ,(when plaindark-theme-org-highlight head4-bg)))))
     `(font-latex-string-face ((,class (:foreground ,str))))
     `(font-latex-warning-face ((,class (:foreground ,war))))

;;;;; ledger-mode
     `(ledger-font-directive-face ((,class (:foreground ,meta))))
     `(ledger-font-posting-amount-face ((,class (:foreground ,yellow))))
     `(ledger-font-posting-date-face ((,class (:foreground ,head1))))
     `(ledger-occur-xact-face ((,class (:background ,bg2))))

;;;;; linum-mode
     `(linum ((,class (:foreground ,lnum :background ,bg2 :inherit default))))

;;;;; line-numbers
     `(line-number ((,class (:foreground ,lnum :background ,bg2 :inherit default))))
     `(line-number-current-line ((,class (:foreground ,base :background ,bg2 :inherit line-number))))

;;;;; linum-relative
     `(linum-relative-current-face ((,class (:foreground ,comp))))

;;;;; lsp
     `(lsp-ui-doc-background ((,class (:background ,bg2))))
     `(lsp-ui-doc-header ((,class (:foreground ,head1 :background ,head1-bg))))

     `(lsp-ui-sideline-code-action ((,class (:foreground ,comp))))

;;;;; magit
     `(magit-blame-culprit ((,class :background ,yellow-bg :foreground ,yellow)))
     `(magit-blame-date    ((,class :background ,yellow-bg :foreground ,green)))
     `(magit-blame-hash    ((,class :background ,yellow-bg :foreground ,func)))
     `(magit-blame-header  ((,class :background ,yellow-bg :foreground ,green)))
     `(magit-blame-heading ((,class :background ,yellow-bg :foreground ,green)))
     `(magit-blame-name    ((,class :background ,yellow-bg :foreground ,yellow)))
     `(magit-blame-sha1    ((,class :background ,yellow-bg :foreground ,func)))
     `(magit-blame-subject ((,class :background ,yellow-bg :foreground ,yellow)))
     `(magit-blame-summary ((,class :background ,yellow-bg :foreground ,yellow :extend t)))
     `(magit-blame-time    ((,class :background ,yellow-bg :foreground ,green)))
     `(magit-branch ((,class (:foreground ,const :inherit bold))))
     `(magit-branch-current ((,class (:background ,blue-bg :foreground ,blue :inherit bold :box t))))
     `(magit-branch-local ((,class (:background ,blue-bg :foreground ,blue :inherit bold))))
     `(magit-branch-remote ((,class (:background ,aqua-bg :foreground ,aqua :inherit bold))))
     `(magit-diff-context-highlight ((,class (:background ,bg2 :foreground ,base :extend t))))
     `(magit-diff-hunk-heading ((,class (:background ,ttip-bg :foreground ,ttip :extend t))))
     `(magit-diff-hunk-heading-highlight ((,class (:background ,ttip-sl :foreground ,base :extend t))))
     `(magit-hash ((,class (:foreground ,var))))
     `(magit-hunk-heading           ((,class (:background ,bg3 :extend t))))
     `(magit-hunk-heading-highlight ((,class (:background ,bg3 :extend t))))
     `(magit-item-highlight ((,class :background ,bg2 :extend t)))
     `(magit-log-author ((,class (:foreground ,func))))
     `(magit-log-head-label-head ((,class (:background ,yellow :foreground ,bg1 :inherit bold))))
     `(magit-log-head-label-local ((,class (:background ,keyword :foreground ,bg1 :inherit bold))))
     `(magit-log-head-label-remote ((,class (:background ,suc :foreground ,bg1 :inherit bold))))
     `(magit-log-head-label-tags ((,class (:background ,magenta :foreground ,bg1 :inherit bold))))
     `(magit-log-head-label-wip ((,class (:background ,cyan :foreground ,bg1 :inherit bold))))
     `(magit-log-sha1 ((,class (:foreground ,str))))
     `(magit-process-ng ((,class (:foreground ,war :inherit bold))))
     `(magit-process-ok ((,class (:foreground ,func :inherit bold))))
     `(magit-reflog-amend ((,class (:foreground ,magenta))))
     `(magit-reflog-checkout ((,class (:foreground ,blue))))
     `(magit-reflog-cherry-pick ((,class (:foreground ,green))))
     `(magit-reflog-commit ((,class (:foreground ,green))))
     `(magit-reflog-merge ((,class (:foreground ,green))))
     `(magit-reflog-other ((,class (:foreground ,cyan))))
     `(magit-reflog-rebase ((,class (:foreground ,magenta))))
     `(magit-reflog-remote ((,class (:foreground ,cyan))))
     `(magit-reflog-reset ((,class (:foreground ,red))))
     `(magit-section-heading        ((,class (:foreground ,keyword :inherit bold :extend t))))
     `(magit-section-highlight      ((,class (:background ,bg2 :extend t))))
     `(magit-section-title ((,class (:background ,bg1 :foreground ,keyword :inherit bold))))

;;;;; man
     `(Man-overstrike ((,class (:foreground ,head1 :inherit bold))))
     `(Man-reverse ((,class (:foreground ,highlight))))
     `(Man-underline ((,class (:foreground ,comp :underline t))))

;;;;; markdown
     `(markdown-header-face-1 ((,class (:inherit bold :foreground ,head1 :height ,(if plaindark-theme-org-height 1.3 1.0) :background ,(when plaindark-theme-org-highlight head1-bg)))))
     `(markdown-header-face-2 ((,class (:inherit bold :foreground ,head2 :height ,(if plaindark-theme-org-height 1.2 1.0) :background ,(when plaindark-theme-org-highlight head2-bg)))))
     `(markdown-header-face-3 ((,class (:bold nil :foreground ,head3 :height ,(if plaindark-theme-org-height 1.1 1.0) :background ,(when plaindark-theme-org-highlight head3-bg)))))
     `(markdown-header-face-4 ((,class (:bold nil :foreground ,head4 :background ,(when plaindark-theme-org-highlight head4-bg)))))
     `(markdown-header-face-5 ((,class (:bold nil :foreground ,head1))))
     `(markdown-header-face-6 ((,class (:bold nil :foreground ,head2))))
     `(markdown-table-face ((,class (:foreground ,base :background ,head1-bg))))

;;;;; meow
     ;; meow-beacon-cursor                         ;; Keypad state cursor.
     ;; meow-insert-cursor                         ;; Insert state cursor.
     ;; meow-keypad-cursor                         ;; Keypad state cursor.
     ;; meow-kmacro-cursor                         ;; Indicator for region direction.
     ;; meow-motion-cursor                         ;; Motion state cursor.
     ;; meow-normal-cursor                         ;; Normal state cursor.
     ;; meow-unknown-cursor                        ;; Unknown state cursor.
     ;; meow-region-cursor-1                       ;; Indicator for region direction.
     ;; meow-region-cursor-2                       ;; Indicator for region direction.
     ;; meow-region-cursor-3                       ;; Indicator for region direction.
     ;; meow-beacon-indicator                      ;; Cursor state indicator.
     ;; meow-insert-indicator                      ;; Insert state indicator.
     ;; meow-keypad-indicator                      ;; Keypad state indicator.
     ;; meow-motion-indicator                      ;; Motion state indicator.
     ;; meow-normal-indicator                      ;; Normal state indicator.
     ;; meow-search-highlight                      ;; Search target highlight.
     ;; meow-search-indicator                      ;; Face for search indicator.
     ;; meow-beacon-fake-cursor                    ;; BEACON selection face.
     ;; meow-cheatsheet-command                    ;; Face for Meow cheatsheet command.
     ;; meow-cheatsheet-highlight                  ;; Face for Meow cheatsheet highlight text.
     ;; meow-beacon-fake-selection                 ;; BEACON selection face.
     ;; meow-position-highlight-number             ;; Num position highlight.
     ;; meow-position-highlight-number-1           ;; Num position highlight.
     ;; meow-position-highlight-number-2           ;; Num position highlight.
     ;; meow-position-highlight-number-3           ;; Num position highlight.
     ;; meow-position-highlight-reverse-number-1   ;; Num position highlight.
     ;; meow-position-highlight-reverse-number-2   ;; Num position highlight.
     ;; meow-position-highlight-reverse-number-3   ;; Num position highlight.

     `(meow-normal-indicator   ((,class (:foreground ,base))))
     `(meow-keypad-indicator   ((,class (:foreground "#801717" :background "#FF6666"))))
     `(meow-insert-indicator   ((,class (:foreground "#309030" :background "#AAE9A0"))))
     `(meow-beacon-indicator   ((,class (:foreground "#6F5033" :background "#FFEE99"))))
     `(meow-motion-indicator   ((,class (:foreground "#505090" :background "#AACCEE"))))
     ;; `(meow-keypad-cursor      ((,class (:background ,yellow))))
     ;; `(meow-insert-cursor      ((,class (:background ,green))))
     ;; `(meow-normal-cursor      ((,class (:background ,magenta))))
     ;; `(meow-motion-cursor      ((,class (:background ,cyan))))
     ;; `(meow-position-highlight-number ((,class (:foreground ,yellow :inherit bold))))
     ;; `(meow-position-highlight-number-1 ((,class (:foreground ,yellow :inherit bold))))
     ;; `(meow-position-highlight-number-2 ((,class (:foreground ,yellow :inherit bold))))
     ;; `(meow-position-highlight-number-3 ((,class (:foreground ,yellow :inherit bold))))
     ;; `(meow-position-highlight-reverse-number-1 ((,class (:foreground ,yellow :inherit bold))))
     ;; `(meow-position-highlight-reverse-number-2 ((,class (:foreground ,yellow :inherit bold))))
     ;; `(meow-position-highlight-reverse-number-3 ((,class (:foreground ,yellow :inherit bold))))
     `(meow-search-highlight ((,class (:background ,aqua))))


     ;; `(meow-normal-cursor
     ;;   '((((class color) (background dark))
     ;; 	  (:inherit cursor))
     ;; 	 (((class color) (background light))
     ;; 	  (:inherit cursor)))
     ;;   "Normal state cursor."
     ;;   :group 'meow)

     ;; `(meow-insert-cursor
     ;;   '((((class color) (background dark))
     ;; 	  (:inherit cursor))
     ;; 	 (((class color) (background light))
     ;; 	  (:inherit cursor)))
     ;;   "Insert state cursor."
     ;;   :group 'meow)

     ;; `(meow-motion-cursor
     ;;   '((((class color) (background dark))
     ;; 	  (:inherit cursor))
     ;; 	 (((class color) (background light))
     ;; 	  (:inherit cursor)))
     ;;   "Motion state cursor."
     ;;   :group 'meow)

     ;; `(meow-keypad-cursor
     ;;   '((((class color) (background dark))
     ;; 	  (:inherit cursor))
     ;; 	 (((class color) (background light))
     ;; 	  (:inherit cursor)))
     ;;   "Keypad state cursor."
     ;;   :group 'meow)

     ;; `(meow-beacon-cursor
     ;;   '((((class color) (background dark))
     ;; 	  (:inherit cursor))
     ;; 	 (((class color) (background light))
     ;; 	  (:inherit cursor)))
     ;;   "Keypad state cursor."
     ;;   :group 'meow)

     ;; `(meow-beacon-cursor
     ;;   '((t (:inherit cursor)))
     ;;   "BEACON cursor face."
     ;;   :group 'meow)

     ;; `(meow-beacon-fake-selection
     ;;   '((t (:inherit region)))
     ;;   "BEACON selection face."
     ;;   :group 'meow)

     ;; `(meow-beacon-fake-cursor
     ;;   '((t (:inherit region :extend nil)))
     ;;   "BEACON selection face."
     ;;   :group 'meow)

     ;; `(meow-unknown-cursor
     ;;   '((((class color) (background dark))
     ;; 	  (:inherit cursor))
     ;; 	 (((class color) (background light))
     ;; 	  (:inherit cursor)))
     ;;   "Unknown state cursor."
     ;;   :group 'meow)

     ;; `(meow-region-cursor-1
     ;;   `((((class color) (background dark)))
     ;; 	 (((class color) (background light))))
     ;;   "Indicator for region direction."
     ;;   :group 'meow)

     ;; `(meow-region-cursor-2
     ;;   `((((class color) (background dark)))
     ;; 	 (((class color) (background light))))
     ;;   "Indicator for region direction."
     ;;   :group 'meow)

     ;; `(meow-region-cursor-3
     ;;   `((((class color) (background dark)))
     ;; 	 (((class color) (background light))))
     ;;   "Indicator for region direction."
     ;;   :group 'meow)

     ;; `(meow-kmacro-cursor
     ;;   `((t (:underline t)))
     ;;   "Indicator for region direction."
     ;;   :group 'meow)

     ;; `(meow-search-highlight
     ;;   '((t (:inherit lazy-highlight)))
     ;;   "Search target highlight."
     ;;   :group 'meow)

     ;; `(meow-position-highlight-number
     ;;   '((((class color) (background dark))
     ;; 	  (:inherit default))
     ;; 	 (((class color) (background light))
     ;; 	  (:inherit default)))
     ;;   "Num position highlight."
     ;;   :group 'meow)

     ;; `(meow-position-highlight-number-1
     ;;   '((t (:inherit meow-position-highlight-number)))
     ;;   "Num position highlight."
     ;;   :group 'meow)

     ;; `(meow-position-highlight-number-2
     ;;   '((t (:inherit meow-position-highlight-number)))
     ;;   "Num position highlight."
     ;;   :group 'meow)

     ;; `(meow-position-highlight-number-3
     ;;   '((t (:inherit meow-position-highlight-number)))
     ;;   "Num position highlight."
     ;;   :group 'meow)

     ;; `(meow-position-highlight-reverse-number-1
     ;;   '((t (:inherit meow-position-highlight-number-1)))
     ;;   "Num position highlight."
     ;;   :group 'meow)

     ;; `(meow-position-highlight-reverse-number-2
     ;;   '((t (:inherit meow-position-highlight-number-2))))

     ;; `(meow-position-highlight-reverse-number-3
     ;;   '((t (:inherit meow-position-highlight-number-3))))

     ;; `(meow-search-indicator
     ;;   '((((class color) (background dark))
     ;; 	  (:foreground "grey40"))
     ;; 	 (((class color) (background light))
     ;; 	  (:foreground "grey60"))))

     ;; `(meow-cheatsheet-command
     ;;   '((((class color) (background dark))
     ;; 	  (:height 0.7 :foreground "grey90"))
     ;; 	 (((class color) (background light))
     ;; 	  (:height 0.7 :foreground "grey10"))))

     ;; `(meow-cheatsheet-highlight
     ;;   '((((class color) (background dark))
     ;; 	  (:foreground "grey90"))
     ;; 	 (((class color) (background light))
     ;; 	  (:foreground "grey10"))))

;;;;; mode-line
     ;; `(mode-line           ((,class (:foreground ,base :background ,act1 :box (:color ,border :line-width 1)))))
     ;; `(mode-line-buffer-id ((,class (:inherit bold :foreground ,blue))))
     ;; `(mode-line-inactive  ((,class (:foreground ,base :background ,bg1  :box (:color ,border :line-width 1)))))
     `(mode-line           ((,class (:foreground ,base :background ,act1 :box nil :underline ,act2 :overline ,act2))))
     `(mode-line-buffer-id ((,class (:inherit bold :foreground ,blue))))
     `(mode-line-inactive  ((,class (:foreground ,base :background ,bg1  :box nil :underline ,act2 :overline ,act2))))

;;;;; mu4e
     `(mu4e-attach-number-face ((,class (:foreground ,var))))
     `(mu4e-cited-1-face ((,class (:foreground ,head1))))
     `(mu4e-cited-2-face ((,class (:foreground ,head2))))
     `(mu4e-cited-3-face ((,class (:foreground ,head3))))
     `(mu4e-cited-4-face ((,class (:foreground ,head4))))
     `(mu4e-cited-5-face ((,class (:foreground ,head1))))
     `(mu4e-cited-6-face ((,class (:foreground ,head2))))
     `(mu4e-cited-7-face ((,class (:foreground ,head3))))
     `(mu4e-contact-face ((,class (:foreground ,func))))
     `(mu4e-draft-face ((,class (:foreground ,var))))
     `(mu4e-flagged-face ((,class (:foreground ,yellow :inherit bold))))
     `(mu4e-header-key-face ((,class (:foreground ,meta :inherit bold))))
     `(mu4e-header-title-face ((,class (:foreground ,keyword :inherit bold))))
     `(mu4e-header-marks-face ((,class (:foreground ,comp))))
     `(mu4e-header-value-face ((,class (:foreground ,keyword :inherit bold))))
     `(mu4e-header-highlight-face ((,class (:background ,highlight))))
     `(mu4e-highlight-face ((,class (:foreground ,comp))))
     `(mu4e-title-face ((,class (:foreground ,head2 :inherit bold))))
     `(mu4e-replied-face ((,class (:foreground ,green))))
     `(mu4e-modeline-face ((,class (:foreground ,yellow))))
     `(mu4e-special-header-value-face ((,class (:foreground ,green))))
     `(mu4e-unread-face ((,class (:foreground ,head1 :inherit bold))))
     `(mu4e-view-url-number-face ((,class (:foreground ,comp))))

;;;;; mu4e-maildirs
     `(mu4e-maildirs-extension-maildir-hl-face ((,class (:foreground ,head1 :inherit bold))))

;;;;; notmuch
     `(notmuch-search-date ((,class (:foreground ,func))))
     `(notmuch-search-flagged-face ((,class (:weight extra-bold))))
     `(notmuch-search-non-matching-authors ((,class (:foreground ,base-dim))))
     `(notmuch-search-unread-face ((,class (:background ,highlight-dim))))
     `(notmuch-tag-face ((,class (:foreground ,keyword))))
     `(notmuch-tag-flagged ((,class (:foreground ,war))))

;;;;; neotree
     `(neo-dir-link-face ((,class (:foreground ,keyword :inherit bold))))
     `(neo-expand-btn-face ((,class (:foreground ,base))))
     `(neo-file-link-face ((,class (:foreground ,base))))
     `(neo-root-dir-face ((,class (:foreground ,func :inherit bold))))

;;;;; org
     `(org-agenda-clocking ((,class (:background ,highlight :foreground ,comp))))
     `(org-agenda-date ((,class (:foreground ,var :height ,(if plaindark-theme-org-agenda-height 1.1 1.0)))))
     `(org-agenda-date-today ((,class (:foreground ,keyword :inherit bold :height ,(if plaindark-theme-org-agenda-height 1.3 1.0)))))
     `(org-agenda-date-weekend ((,class (:inherit bold :foreground ,var))))
     `(org-agenda-done ((,class (:foreground ,suc :height ,(if plaindark-theme-org-agenda-height 1.2 1.0)))))
     `(org-agenda-structure ((,class (:inherit bold :foreground ,comp))))
     `(org-block ((,class (:background ,cblk-bg :foreground ,cblk :extend t))))
     `(org-block-begin-line ((,class (:background ,cblk-ln-bg :foreground ,cblk-ln :extend t))))
     `(org-block-end-line ((,class (:background ,cblk-ln-bg :foreground ,cblk-ln :extend t))))
     `(org-clock-overlay ((,class (:foreground ,comp))))
     `(org-code ((,class (:foreground ,cyan))))
     `(org-column ((,class (:background ,highlight :inherit ,(if plaindark-theme-org-height 'default)))))
     `(org-column-title ((,class (:background ,highlight))))
     `(org-date ((,class (:underline t :foreground ,var))))
     `(org-date-selected ((,class (:background ,func :foreground ,bg1))))
     `(org-document-info-keyword ((,class (:foreground ,meta))))
     `(org-document-title ((,class (:foreground ,func :inherit bold :height ,(if plaindark-theme-org-height 1.4 1.0) :underline t))))
     `(org-done ((,class (:foreground ,suc :inherit bold :background ,green-bg))))
     `(org-ellipsis ((,class (:foreground ,keyword))))
     `(org-footnote  ((,class (:underline t :foreground ,base))))
     `(org-headline-done ((,class (:foreground ,aqua))))
     `(org-headline-todo ((,class (:foreground ,meta))))
     `(org-hide ((,class (:foreground ,base))))
     `(org-kbd ((,class (:inherit region :foreground ,base :box (:line-width 1 :style released-button)))))
     `(org-level-1 ((,class (:inherit bold :bold ,(if plaindark-theme-org-bold 'unspecified nil) :foreground ,head1 :height ,(if plaindark-theme-org-height 1.2 1.0) :background ,(when plaindark-theme-org-highlight head1-bg)))))
     `(org-level-2 ((,class (:bold nil :foreground ,head2 :height ,(if plaindark-theme-org-height 1.1 1.0) :background ,(when plaindark-theme-org-highlight head2-bg)))))
     `(org-level-3 ((,class (:bold nil :foreground ,head3 :height ,(if plaindark-theme-org-height 1.05 1.0) :background ,(when plaindark-theme-org-highlight head3-bg)))))
     `(org-level-4 ((,class (:bold nil :foreground ,head4 :height ,(if plaindark-theme-org-height 1.0 1.0) :background ,(when plaindark-theme-org-highlight head4-bg)))))
     `(org-level-5 ((,class (:bold nil :foreground ,head1 :height ,(if plaindark-theme-org-height 1.1 1.0)))))
     `(org-level-6 ((,class (:bold nil :foreground ,head2 :height ,(if plaindark-theme-org-height 1.1 1.0)))))
     `(org-level-7 ((,class (:bold nil :foreground ,head3 :height ,(if plaindark-theme-org-height 1.1 1.0)))))
     `(org-level-8 ((,class (:bold nil :foreground ,head4 :height ,(if plaindark-theme-org-height 1.1 1.0)))))
     `(org-link ((,class (:underline t :foreground ,comment))))
     `(org-meta-line ((,class (:foreground ,meta))))
     `(org-mode-line-clock-overrun ((,class (:foreground ,err))))
     `(org-priority ((,class (:foreground ,war :inherit bold :bold ,(if plaindark-theme-org-priority-bold 'unspecified nil)))))
     `(org-quote ((,class (:inherit org-block :slant italic))))
     `(org-scheduled ((,class (:foreground ,comp))))
     `(org-scheduled-today ((,class (:foreground ,func :height ,(if plaindark-theme-org-agenda-height 1.2 1.0)))))
     `(org-scheduled-previously ((,class (:foreground ,base :slant italic))))
     `(org-sexp-date ((,class (:foreground ,base))))
     `(org-special-keyword ((,class (:foreground ,func))))
     `(org-table ((,class (:foreground ,base :background ,head1-bg))))
     `(org-tag ((,class (:foreground ,meta))))
     `(org-time-grid ((,class (:foreground ,str))))
     `(org-todo ((,class (:foreground ,war :inherit bold :background ,yellow-bg))))
     `(org-upcoming-deadline ((,class (:foreground ,war :inherit org-priority))))
     `(org-upcoming-distant-deadline ((,class (:foreground ,suc :inherit org-priority))))
     `(org-verbatim ((,class (:foreground ,keyword))))
     `(org-verse ((,class (:inherit org-block :slant italic))))
     `(org-warning ((,class (:foreground ,err :inherit org-priority))))

;;;;; outline
     `(outline-1 ((,class (:inherit org-level-1))))
     `(outline-2 ((,class (:inherit org-level-2))))
     `(outline-3 ((,class (:inherit org-level-3))))
     `(outline-4 ((,class (:inherit org-level-4))))
     `(outline-5 ((,class (:inherit org-level-5))))
     `(outline-6 ((,class (:inherit org-level-6))))
     `(outline-7 ((,class (:inherit org-level-7))))
     `(outline-8 ((,class (:inherit org-level-8))))

;;;;; parinfer
     `(parinfer-pretty-parens:dim-paren-face ((,class (:foreground ,base-dim))))

;;;;; parinfer-rust-mode
     `(parinfer-rust-dim-parens ((,class (:foreground ,base-dim))))

;;;;; perspective
     `(persp-selected-face ((,class (:inherit bold :foreground ,func))))

;;;;; popup
     `(popup-enu-selection-face ((,class (:background ,ttip-sl :foreground ,base))))
     `(popup-face ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(popup-isearch-match ((,class (:inherit match))))
     `(popup-menu-face ((,class (:background ,ttip-bg :foreground ,base))))
     `(popup-menu-mouse-face ((,class (:inherit highlight))))
     `(popup-scroll-bar-background-face ((,class (:background ,bg2))))
     `(popup-scroll-bar-foreground-face ((,class (:background ,act2))))
     `(popup-tip-face ((,class (:background ,ttip-sl :foreground ,base :bold nil :italic nil :underline nil))))

;;;;; powerline
     `(powerline-active1 ((,class (:background ,act2 :foreground ,base))))
     `(powerline-active2 ((,class (:background ,act2 :foreground ,base))))
     `(powerline-inactive1 ((,class (:background ,bg2 :foreground ,base))))
     `(powerline-inactive2 ((,class (:background ,bg2 :foreground ,base))))

;;;;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face ((,class :foreground ,base-dim)))
     `(rainbow-delimiters-depth-2-face ((,class :foreground ,blue-dim)))
     `(rainbow-delimiters-depth-3-face ((,class :foreground ,cyan-dim)))
     `(rainbow-delimiters-depth-4-face ((,class :foreground ,green-dim)))
     `(rainbow-delimiters-depth-5-face ((,class :foreground ,yellow-dim)))
     `(rainbow-delimiters-depth-6-face ((,class :foreground ,blue-dim)))
     `(rainbow-delimiters-depth-7-face ((,class :foreground ,cyan-dim)))
     `(rainbow-delimiters-depth-8-face ((,class :foreground ,green-dim)))
     `(rainbow-delimiters-mismatched-face ((,class :foreground ,err :overline t)))
     `(rainbow-delimiters-unmatched-face ((,class :foreground ,err :overline t)))

;;;;; rcirc
     `(rcirc-bright-nick ((,class (:background ,aqua-bg :foreground ,cyan))))
     `(rcirc-dim-nick ((,class (:foreground ,base-dim))))
     `(rcirc-keyword ((,class (:background ,green-bg-s :foreground ,green))))
     `(rcirc-timestamp ((,class (:foreground ,keyword))))
     `(rcirc-track-keyword ((,class (:background ,green :foreground ,bg1))))
     `(rcirc-url ((,class (:inherit link))))

;;;;; sh-mode
     `(sh-heredoc ((,class :foreground ,str)))
     `(sh-quoted-exec ((,class :foreground ,func)))

;;;;; shm
     `(shm-current-face ((,class (:background ,green-bg-s))))
     `(shm-quarantine-face ((,class (:background ,red-bg-s))))

;;;;; show-paren
     `(show-paren-match ((,class (:foreground ,mat :inherit bold  :underline ,(when plaindark-theme-underline-parens t)))))
     `(show-paren-match-expression ((,class (:background ,green-bg-s))))
     `(show-paren-mismatch ((,class (:foreground ,err :inherit bold :underline ,(when plaindark-theme-underline-parens t)))))

;;;;; smartparens
     `(sp-pair-overlay-face ((,class (:background ,highlight :foreground nil))))
     `(sp-show-pair-match-face ((,class (:foreground ,mat :inherit bold  :underline ,(when plaindark-theme-underline-parens t)))))

;;;;; smerge
     `(smerge-base ((,class (:background ,yellow-bg :extend t))))
     `(smerge-markers ((,class (:background ,ttip-bg :foreground ,ttip :extend t))))
     `(smerge-mine ((,class (:background ,red-bg))))
     `(smerge-other ((,class (:background ,green-bg))))
     `(smerge-refined-added ((,class (:background ,green-bg-s :foreground ,green))))
     `(smerge-refined-changed ((,class (:background ,blue-bg-s :foreground ,blue))))
     `(smerge-refined-removed ((,class (:background ,red-bg-s :foreground ,red))))

;;;;; solaire
     `(solaire-default-face ((,class (:inherit default :background ,bg2))))
     `(solaire-minibuffer-face ((,class (:inherit default :background ,bg2))))
     `(solaire-hl-line-face ((,class (:inherit hl-line :background ,bg2))))
     `(solaire-org-hide-face ((,class (:inherit org-hide :background ,bg2))))

;;;;; spaceline
     `(spaceline-flycheck-error  ((,class (:foreground ,err))))
     `(spaceline-flycheck-info   ((,class (:foreground ,keyword))))
     `(spaceline-flycheck-warning((,class (:foreground ,war))))
     `(spaceline-python-venv ((,class (:foreground ,comp))))

;;;;; plaindark-specific
     `(plaindark-transient-state-title-face ((,class (:background nil :foreground ,comp :box nil :inherit bold))))

;;;;; swiper
     `(swiper-line-face ((,class (:background ,highlight :inherit bold))))
     `(swiper-match-face-1 ((,class (:inherit bold))))
     `(swiper-match-face-2 ((,class (:foreground ,head1 :underline t))))
     `(swiper-match-face-3 ((,class (:foreground ,head4 :underline t))))
     `(swiper-match-face-4 ((,class (:foreground ,head3 :underline t))))

;;;;; tabbar
     `(tabbar-button ((,class (:inherit tabbar-default ))))
     `(tabbar-button-highlight ((,class (:inherit tabbar-default))))
     `(tabbar-default ((,class (:background ,bg1 :foreground ,head1 :height 0.9))))
     `(tabbar-highlight ((,class (:underline t))))
     `(tabbar-selected ((,class (:inherit tabbar-default :foreground ,func :weight bold))))
     `(tabbar-selected-modified ((,class (:inherit tabbar-default :foreground ,red :weight bold))))
     `(tabbar-separator ((,class (:inherit tabbar-default))))
     `(tabbar-unselected ((,class (:inherit tabbar-default :background ,bg1 :slant italic :weight light))))
     `(tabbar-unselected-modified ((,class (:inherit tabbar-unselected :background ,bg1 :foreground ,red))))

;;;;; term
     `(term ((,class (:foreground ,base :background ,bg1))))
     `(term-color-black ((,class (:foreground ,bg4 :background ,bg4))))
     `(term-color-blue ((,class (:foreground ,keyword :background ,keyword))))
     `(term-color-cyan ((,class (:foreground ,cyan :background ,cyan))))
     `(term-color-green ((,class (:foreground ,green :background ,green))))
     `(term-color-magenta ((,class (:foreground ,magenta :background ,magenta))))
     `(term-color-red ((,class (:foreground ,red :background ,red))))
     `(term-color-white ((,class (:foreground ,base :background ,base))))
     `(term-color-yellow ((,class (:foreground ,yellow :background ,yellow))))

;;;;; vterm
     `(vterm-color-default ((,class (:foreground ,base :background ,bg1))))
     ;; vterm-color-black used to render black color code.
     ;; The foreground color is used as ANSI color 0 and the background
     ;; color is used as ANSI color 8.
     `(vterm-color-black ((,class (:foreground ,bg4 :background ,bg4))))
     `(vterm-color-blue ((,class (:foreground ,blue :background ,blue))))
     `(vterm-color-cyan ((,class (:foreground ,cyan :background ,cyan))))
     `(vterm-color-green ((,class (:foreground ,green :background ,green))))
     `(vterm-color-magenta ((,class (:foreground ,magenta :background ,magenta))))
     `(vterm-color-red ((,class (:foreground ,red  :background ,red))))
     `(vterm-color-white ((,class (:foreground ,base  :background ,base))))
     `(vterm-color-yellow ((,class (:foreground ,yellow   :background ,yellow))))

;;;;; tide
     `(tide-hl-identifier-face ((,class (:foreground ,yellow :background ,yellow-bg))))

;;;;; treemacs
     `(treemacs-git-added-face ((,class (:foreground ,green :background ,green-bg))))
     `(treemacs-git-conflict-face ((,class (:foreground ,red :background ,red-bg))))
     `(treemacs-git-ignored-face ((,class (:foreground ,yellow))))
     `(treemacs-git-modified-face ((,class (:foreground ,blue :background ,blue-bg))))
     `(treemacs-git-untracked-face ((,class (:foreground ,aqua :background ,aqua-bg))))

;;;;; tab-bar-mode
     `(tab-bar ((,class (:foreground ,base :background ,bg1))))
     `(tab-bar-tab ((,class (:foreground ,base :background ,bg1 :weight bold))))
     `(tab-bar-tab-inactive ((,class (:foreground ,base-dim :background ,bg2 :weight light))))

;;;;; tab-line-mode
     `(tab-line ((,class (:foreground ,base :background ,bg1))))
     `(tab-line-tab-current ((,class (:foreground ,base :background ,bg1 :weight bold))))
     `(tab-line-tab-inactive ((,class (:foreground ,base-dim :background ,bg2 :weight light))))

;;;;; web-mode
     `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
     `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
     `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
     `(web-mode-current-element-highlight-face ((,class (:background ,bg3))))
     `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
     `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
     `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
     `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
     `(web-mode-html-tag-face ((,class (:foreground ,keyword))))
     `(web-mode-keyword-face ((,class (:foreground ,keyword))))
     `(web-mode-string-face ((,class (:foreground ,str))))
     `(web-mode-symbol-face ((,class (:foreground ,type))))
     `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
     `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))

;;;;; which-key
     `(which-key-command-description-face ((,class (:foreground ,base))))
     `(which-key-group-description-face ((,class (:foreground ,keyword))))
     `(which-key-key-face ((,class (:foreground ,func :inherit bold))))
     `(which-key-separator-face ((,class (:background nil :foreground ,str))))
     `(which-key-special-key-face ((,class (:background ,func :foreground ,bg1))))

;;;;; which-function-mode
     `(which-func ((,class (:foreground ,func))))

;;;;; whitespace-mode
     `(whitespace-empty ((,class (:background nil :foreground ,yellow))))
     `(whitespace-indentation ((,class (:background nil :foreground ,war))))
     `(whitespace-line ((,class (:background nil :foreground ,comp))))
     `(whitespace-newline ((,class (:background nil :foreground ,comp))))
     `(whitespace-space ((,class (:background nil :foreground ,act2))))
     `(whitespace-space-after-tab ((,class (:background nil :foreground ,yellow))))
     `(whitespace-space-before-tab ((,class (:background nil :foreground ,yellow))))
     `(whitespace-tab ((,class (:background nil :foreground ,act2))))
     `(whitespace-trailing ((,class (:background ,err :foreground ,war))))

;;;;; other, need more work
     `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
     `(ffap ((,class (:foreground ,base))))
     `(flx-highlight-face ((,class (:foreground ,comp :underline nil))))
     `(icompletep-determined ((,class :foreground ,keyword)))
     `(js2-external-variable ((,class (:foreground ,comp))))
     `(js2-function-param ((,class (:foreground ,const))))
     `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
     `(js2-jsdoc-html-tag-name ((,class (:foreground ,keyword))))
     `(js2-jsdoc-value ((,class (:foreground ,str))))
     `(js2-private-function-call ((,class (:foreground ,const))))
     `(js2-private-member ((,class (:foreground ,base))))
     `(js3-error-face ((,class (:underline ,war))))
     `(js3-external-variable-face ((,class (:foreground ,var))))
     `(js3-function-param-face ((,class (:foreground ,keyword))))
     `(js3-instance-member-face ((,class (:foreground ,const))))
     `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
     `(js3-warning-face ((,class (:underline ,keyword))))
     `(slime-repl-inputed-output-face ((,class (:foreground ,comp))))
     `(trailing-whitespace ((,class :foreground nil :background ,err)))
     `(undo-tree-visualizer-current-face ((,class :foreground ,keyword)))
     `(undo-tree-visualizer-default-face ((,class :foreground ,base)))
     `(undo-tree-visualizer-register-face ((,class :foreground ,comp)))
     `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var))))

    (custom-theme-set-variables
     theme-name

;;;;; ansi-color-names
     `(ansi-color-names-vector [,bg4 ,red ,green ,yellow ,blue ,magenta ,cyan ,base])

;;;;; hl-todo
     `(hl-todo-keyword-faces '(("WAIT"        . ,war)
                               ("HOLD"        . ,war)
                               ("REVIEW"      . ,war)
			       ("ACTIVE"      . ,war)
                               ("FIXME"       . ,war)
                               ("XXX+"        . ,war)
			       ("\\?\\?\\?+"     . ,war)
                               ("PLAN"        . ,aqua)
			       ("NEXT"        . ,aqua)
			       ("BACKLOG"     . ,aqua)
                               ("TODO"        . ,blue)
                               ("READY"       . ,blue)
                               ("OKAY"        . ,blue)
                               ("DONT"        . ,red)
                               ("FAIL"        . ,red)
			       ("CANC"        . ,red)
                               ("DONE"        . ,suc)
			       ("COMPLETED"   . ,suc)
                               ("NOTE"        . ,yellow)
                               ("HACK"        . ,yellow)
                               ("TEMP"        . ,yellow)))
;;;;; org
     `(org-fontify-done-headline nil)
     `(org-fontify-todo-headline nil)

;;;;; pdf-tools
     `(pdf-view-midnight-colors '(,base . ,bg1)))
    ))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(deftheme plain-dark "Ditch the rainbow of distraction.")

(create-plaindark-theme 'plain-dark)

(provide-theme 'plain-dark)

;;; plain-dark-theme.el ends here
