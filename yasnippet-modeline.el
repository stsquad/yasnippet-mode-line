;;; yasnippet-modeline.el --- Update mode-line with potential YASnippet keywords

;; Copyright (C) 2013       Alex Bennée

;; Author: Alex Bennée <alex@bennee.com>
;; Maintainer: Alex Bennée <alex@bennee.com>
;; Version: 0.01
;; Homepage: https://gist.github.com/4664006

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides a formatted string suitable for placing in the mode-line to
;; show what YASnippet completions are available. For example if point is next
;; to the partial string xy and there are the potential completions xyzzy, xyy and xya
;; the string should be rendered as <bold>xy</bold><italic>ayz<italic>.
;;
;; This hopefully provides an aide memoir when you can't remember what your abbreviation
;; for a snippet was.

;;; Code:

(defun ajb-current-yasnippet-candiates (chars-so-far)
  "Return the current keys and a list of next keys that would make
yasnippet completions based on chars-so-far"
  (let ((keys (yas-active-keys))
	(candidates '()))
    (mapc '(lambda (k)
	     (when (string-prefix-p chars-so-far k)
	       (add-to-list 'candidates k)))
	  keys)
    candidates))

;; Tests (assume current default Elisp mode snippets)
; (ajb-current-yasnippet-candiates "b")
; => ("bs" "bsnp" "bmp" "bfn" "botap" "bol" "bc")


(defun ajb-next-yasnippet-keys (chars-so-far list-of-abbrevs)
  "Return a string containing all the characters that can be used
after CHARS-SO-FAR which would still be valid completions"
  (let ((l (length chars-so-far))
	(next-chars '()))
    (mapc
     '(lambda (ab)
	(when (> (length ab) l)
	  (add-to-list 'next-chars (aref ab l))))
     list-of-abbrevs)
    (concat (sort next-chars '<))))

;; Tests (assume current default Elisp mode snippets)
; (ajb-next-yasnippet-keys "xy" '("xyzzy" "zyy" "xya" "xy" "x"))
; => "ayz"
; (ajb-next-yasnippet-keys "" (yas-active-keys))
; => "abcdefgiklmnoprstuvwxy"

(defun ajb-format-yasnippet-options (&optional prefix)
  "Return a format string showing the possible next keys that will
  still be yasnippet completions. If no PREFIX is passed then it will
  try to find it with word-at-point"
  
  (let ((word (if prefix
		  prefix
		(save-excursion
		(backward-char)
		(word-at-point))))
	(fmt "None"))
    (if (not (stringp word))
	(setq fmt
	      (propertize
	       (ajb-next-yasnippet-keys
		""
		(yas-active-keys))
	       'face 'italic
	       ))
      (let ((next-chars (ajb-next-yasnippet-keys
			 word
			 (ajb-current-yasnippet-candiates word))))
	(when (> (length next-chars) 0)
	  (setq fmt
		(concat
		 (propertize word 'face 'bold)
		 (propertize next-chars
			     'face 'italic
			     'help-echo (ajb-current-yasnippet-candiates word)))))))
    "this works"))

;; Tests
; (ajb-format-yasnippet-options "b")
; => #("bcfmos" 0 1 (face bold) 1 6 (face italic help-echo ("bs" "bsnp" "bmp" "bfn" "botap" "bol" "bc")))
