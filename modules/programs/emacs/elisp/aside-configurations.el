;;; aside-configurations.el -*- lexical-binding: t -*-

;; Copyright 2021 Matt Beshara

;; Author: Matt Beshara <m@mfa.pw>
;; URL: https://git.sr.ht/~mfa/aside
;; Version: 2.6.8
;; Package-Requires: ((emacs "27.1") (dash "2.19.1"))
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This file contains Aside configurations simple enough to be defined by the
;; ‘aside-define-configuration’ macro alone.

;; These configurations can be activated by doing something along these lines:
;; (require 'aside-configurations)
;; (aside-enable-configuration 'messages)
;; (define-key global-map (kbd "C-S-x") 'aside-messages-dwim)
;; (aside-enable-configuration 'context)
;; (define-key global-map (kbd "C-S-z") 'aside-context-dwim)


;;; Code:

(require 'rx)

;; This configuration displays *Messages*, compile logs, and
;; other related information in a side window displayed at the
;; bottom of a frame.  It’s also convenient to use for displaying
;; search results, as the right sidebasr is too thin to display
;; lines which are very long.  This window will appear to the
;; right of the Aside-Vterm window when both are visible.
(aside-define-configuration messages
  (rx (or
       ;; Messages
       "*Agenda Commands*"
       "*Async-native-compile-log*"
       "*Async Shell Command*"
       "*Backtrace*"
       "*compilation*"
       "*Compile-Log*"
       "*Geiser Debug*"
       "*Messages*"
       "*Native-compile-Log*"
       "*sly-compilation*"
       "*sly-error"
       "*Warning*"
       "*Warnings*"
       ;; Search results
       (seq "*Embark Export" (one-or-more nonl) "grep")
       (seq "*Occur" (one-or-more nonl))
       "*grep*"
       "*PDF-Occur*"
       (seq "*rg" (one-or-more nonl))
       "*trace-output*"))
  '((side . bottom)
    (slot . 5)
    (window-height . 10))
  '())

;; The Context configuration causes a side window on the right of the frame
;; to be used for buffers which provide contextual information that I only
;; want visible for a short amount of time, such as Help, Apropos, Occur, etc.
;; Having one of these buffers appear or disappear should not cause me to lose
;; my place in any other buffer, but the window they appear in should be big
;; enough to let me see the information I need with a minimum of scrolling.
(aside-define-configuration context
  (lambda (buffer-or-name &optional _)
    (let ((buffer (get-buffer buffer-or-name)))
      (and (not (string-match-p
             (rx (seq "*Embark Export" (one-or-more nonl) "grep"))
             (buffer-name buffer)))
         (string-match-p
          (rx (or "*Apropos*"
                 (seq "*cider-inspect" (one-or-more nonl))
                 "*Chicken Documentation*"
                 "*Currency*"
                 (seq "*Dictionary" (one-or-more nonl))
                 (seq "*eldoc" (one-or-more nonl))
                 "*Embark Actions*"
                 (seq "*Embark Collect" (one-or-more nonl))
                 (seq "*Embark Export" (one-or-more nonl))
                 "*Geiser documentation*"
                 (seq bol (seq "*Help" (one-or-more nonl)))
		 "*info*"
                 "*lingva*"
		 "*lsp-help*"
                 "*Metahelp*"
                 (seq "*Shortdoc " (one-or-more nonl))
                 (seq "*sly-apropos" (one-or-more nonl))
                 "*sly-db"
                 "*sly-description*"
                 (seq "*sly-inspector" (one-or-more nonl))
                 (seq "*sly-xref" (one-or-more nonl))
                 "*Synonyms List*"
                 "*wclock*"
		 (seq "*WoMan" (one-or-more nonl))
                 "*WordNut*"
                 "*xref*"))
          (buffer-name buffer)))))
  '((side . right)
    (window-width . 80))
  '(aside-hook-disable-display-line-numbers-mode))

;; A dumb ‘sticky note’ buffer displayed below the Context window.  Useful for
;; comparing two Help buffers, because you can easily keep a copy of the
;; content of one Help buffer visible while using the Context window for
;; browsing another Help buffer.
(aside-define-configuration sticky
  "\\*Aside-Sticky\\*"
  '((side . right)
    (slot . 5)
    (window-width . 80))
  '(aside-hook-disable-display-line-numbers-mode)
  nil
  #'aside--sticky)

(defun aside--sticky ()
  "Create the *Aside-Sticky* buffer."
  (interactive)
  (select-window (display-buffer (get-buffer-create "*Aside-Sticky*")))
  (text-mode)
  (display-line-numbers-mode -1))

(aside-enable-configuration 'messages)
(define-key global-map (kbd "M-m") 'aside-messages-dwim)
(aside-enable-configuration 'context)
(define-key global-map (kbd "C-S-p") 'aside-context-dwim)

;; (provide 'aside-configurations)

;;; aside-configurations.el ends here
