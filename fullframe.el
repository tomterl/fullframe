;;; fullframe.el --- Advice commands to execute fullscreen, restoring the window setup when exiting.

;; Copyright (C) 2013 Tom Regner

;; Author: Tom Regner <tom@goochesa.de>
;; Maintainer: Tom Regner <tom@goochesa.de>
;; Version: 0.0.1
;; Keywords: fullscreen

;;  This file is NOT part of GNU Emacs

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This is a library that package developers can use to provide user
;; friendly fullscreen execution of buffer exposing commands, as well
;; as to use in personal emacs configurations to attain the same goal
;; for packages that don't use fullframe themself.
;;
;; Example:
;; - Open magit-status in a single window in fullscreen
;;   (require 'fullframe)
;;   (fullframe magit-status magit-mode-quit-window :magit-fullscreen nil)
;; 
;;; Code:

;; customization
;; - none

;; variables 
(defvar efull/rule-no 1
  "Used to generate unique advice names that are traceable to
  this module.")

;; internal functions
;; - none

;; API
;;;###autoload
(defmacro fullframe (command-on command-off register &optional kill-on-coff)
  "Advice execution of COMMAND-ON to store the current window
  state in REGISTER and display a single frame. Advice COMMAND-OFF to
  restore the state stored in REGISTER. If KILL-ON-COFF is true,
  kill-buffer is called on command-off."
  `(progn
     (defadvice ,command-on (around
                             ,(make-symbol
                               (concat "fullscreen-rule-" (number-to-string efull/rule-no)))
                             activate)
       (window-configuration-to-register ,register)
       ad-do-it
       (delete-other-windows))
     (if ,kill-on-coff
         (defadvice ,command-off (after
                                  ,(make-symbol
                                    (concat "restore-setup-rule-" (number-to-string efull/rule-no)))
                                  activate)
           (kill-buffer)
           (jump-to-register ,register))
       (defadvice ,command-off (after
                                ,(make-symbol
                                  (concat "restore-setup-rule-" (number-to-string efull/rule-no)))
                                activate)
         (jump-to-register ,register)))
     (setq efull/rule-no (+ efull/rule-no 1))))

;; interactive functions
;; - none

(provide 'fullframe)
;;; fullframe.el ends here
