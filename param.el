
(defun edit-param-toggle (tagname tagval)
  "Find TAGNAME and set it to the default TAGVAL if it doesn't exist. Or toggle TAGVAL if it does."
  (save-excursion
    (jump-to-param-start)
    (let* ((tags (get-current-subtags))
           (has-tag (member tagname tags))
           (is-true nil)
           (just-inserted-true nil))
      (if has-tag
        (setq is-true
              (progn (re-search-forward (format "%s=\"\\([^\"]+\\)\"" tagname))
                     (string= tagval (match-string-no-properties 1))))
        ;; - Create checked=true
        (jump-to-start-of-tag)
        (search-forward "label=")
        (backward-char 6)
        (insert (format "%s=\"%s\" " tagname tagval))
        (setq just-inserted-true t))
      ;; -------
      ;; At this point there is a tagname=(true|false)
      (unless just-inserted-true
        (jump-to-start-of-tag)
        (search-forward (format "%s=\"" tagname))
        (let* ((end (search-forward "\""))
               (beg (search-backward (format "%s=\"" tagname))))
          ;; Remove current
          (delete-region beg end)
          ;; Insert new
          (insert (format "%s=\"%s\"" tagname
                          (if is-true "false" "true"))))))))


(defun edit-param-toggle-optional (&optional point)
  (interactive "P")
  (edit-param-toggle "optional" "true"))

