;;; fullframe.el --- Generalized automatic execution in a single frame

;; Copyright (C) 2013 Tom Regner

;; Author: Tom Regner <tom@goochesa.de>
;; Maintainer: Tom Regner <tom@goochesa.de>
;; Version: 0.0.2
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
;; Generalized automatic execution in a single frame
;;
;; This is a library that package developers can use to provide user
;; friendly single window per frame execution of buffer exposing
;; commands, as well as to use in personal emacs configurations to attain
;; the same goal for packages that don't use =fullframe= or the likes of
;; it themself.
;;
;;  Example: Setup =magit-status= to open in one window in the current
;;  frame when called
;; Example:
;; - Open magit-status in a single window in fullscreen
;;   (require 'fullframe)
;;   (fullframe magit-status magit-mode-quit-window :magit-fullscreen nil)
;;
;;; Code:

;; customization
;; - none

;; variables
;; - none

;; internal functions
;; - none

;; API
;;;###autoload
(defmacro fullframe (command-on command-off register &optional kill-on-coff)
  "Advice execution of COMMAND-ON to store the current window
  state in REGISTER and display a single frame. Advice COMMAND-OFF to
  restore the state stored in REGISTER. If KILL-ON-COFF is true,
  kill-buffer is called on command-off."
  (let* ((on-rule-name (gensym "fullscreen-rule-"))
         (off-rule-name (gensym "restore-setup-rule-"))
         (register-name (gensym))
         (off-code (if kill-on-coff
                       `(progn
                          (kill-buffer)
                          (jump-to-register ,register-name))
                     `(jump-to-register ,register-name))))
    `(let ((,register-name ,register))
       (defadvice ,command-on (around ,on-rule-name activate)
         (window-configuration-to-register ,register-name)
         ad-do-it
         (delete-other-windows))
       (defadvice ,command-off (after ,off-rule-name activate)
         ,off-code))))

;; interactive functions
;; - none

(provide 'fullframe)
;;; fullframe.el ends here
