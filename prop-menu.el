;;; prop-menu.el --- Create and display a context menu based on text and overlay properties  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  David Raymond Christiansen

;; Author: David Christiansen <david@davidchristiansen.dk>
;; URL: https://github.com/david-christiansen/prop-menu-el
;; Package-Requires:  ((emacs "24") (cl-lib "0.5"))
;; Version: 0.1
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a library for computing context menus based on text
;; properties and overlays. The intended use is to have tools that
;; annotate source code and others that use these annotations, without
;; requiring a direct coupling between them, but maintaining
;; discoverability.

;; Major modes that wish to use this library should first define an
;; appropriate value for `prop-menu-item-functions'. Then, they should
;; bind `prop-menu-by-completing-read' to an appropriate
;; key. Optionally, a mouse pop-up can be added by binding
;; `prop-menu-show-menu' to a mouse event.

;;; Code:
(require 'cl-lib)

(defun prop-menu--merge-plists (plists)
  "Merge PLISTS, resolving conflicts to the left."
  (let ((res (pop plists))
        this-plist k v)
    (while plists
      (setq this-plist (pop plists))
      (while this-plist
        (setq k (pop this-plist))
        (setq v (pop this-plist))
        (unless (plist-get res k)
          (plist-put res k v))))
    res))

(defvar prop-menu-item-functions nil
  "A list of functions to compute menu items from text and overlay properties.

Each function should take a plist as its argument and return a
list of menu items. A menu item consists of a string to be
displayed to the user and a command to be executed if that item
is selected. Separators can be added by using \"--\" as the string.

Major modes that provide context menus are expected to populate
this variable with appropriate functions.")
(make-variable-buffer-local 'prop-menu-item-functions)

(let ((counter 0))
  (defun prop-menu--unique-val ()
    (cl-incf counter)))

(defun prop-menu--items-for-location (where)
  "Return the menu items based on the text properties and overlays at WHERE."
  (let* ((text-props (text-properties-at where))
         (overlays (overlays-at where t))
         (overlay-props-list (mapcar #'overlay-properties overlays))
         (props (prop-menu--merge-plists (cons text-props overlay-props-list))))
    (apply #'append
           (cl-loop for fun in prop-menu-item-functions
                    collecting (funcall fun props)))))

(defun prop-menu-by-completing-read (where)
  "Show a text menu for WHERE, based on the text properties and overlays.

When called interactively, WHERE defaults to point."
  (interactive "d")
  (let ((menu-items (prop-menu--items-for-location where)))
    (when menu-items
      (let ((selection (completing-read "Command: " menu-items nil t)))
        (when selection
          (let ((cmd (assoc selection menu-items)))
            (when cmd (funcall (cadr cmd)))))))))

(defun prop-menu-show-menu (click)
  "Show a menu based on the location of CLICK, computed from the value of `prop-menu-item-functions'.

When calling `prop-menu-item-functions', point is at the clicked
location."
  (interactive "e")
  (let* ((where (posn-point (event-end click)))
         (menu-items (save-excursion
                       (goto-char where)
                       (prop-menu--items-for-location where))))
    (when menu-items
      (let* ((menu (make-sparse-keymap))
             (todo (cl-loop for (str action) in menu-items
                            collecting (let ((sym (prop-menu--unique-val)))
                                         (define-key-after menu `[,sym]
                                           `(menu-item ,str (lambda () (interactive)))
                                           t)
                                         (cons sym action))))
             (selection (x-popup-menu t menu)))
        (when selection
          (funcall (cdr (assoc (car selection) todo))))))))

(provide 'prop-menu)
;;; prop-menu.el ends here
