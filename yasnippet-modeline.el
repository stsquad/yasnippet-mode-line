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

(defun ajb-format-yasnippet-options-at-point ()
  "Return a format string showing the possible next keys that will
  still be yasnippet completions."
  (let ((word (save-excursion
		(backward-char)
		(word-at-point)))
	(fmt "...none"))
    (when (stringp word)
      (let ((next-chars (ajb-next-yasnippet-keys
			 word
			 (ajb-current-yasnippet-candiates word))))
	(setq fmt (format "%s:%s" word next-chars))))
    fmt))
