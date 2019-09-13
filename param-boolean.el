(define-minor-mode param-mode-boolean
  "This mode can overlay conditional-mode"
  :init-value nil
  :lighter "boolean "
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t b c") 'edit-param-toggle-checked)
    (define-key map (kbd "C-t b t") 'edit-param-truevalue)
    (define-key map (kbd "C-t b f") 'edit-param-falsevalue)
    map))

(defun insert-param-boolean (&optional main-tag)
  (interactive (list (find-valid-parent)))
  (jump-to-param-end)
  (insert "\n")
  (execute-kbd-macro (read-kbd-macro "TAB"))
  (cond ((string= main-tag "inputs") (call-interactively 'insert-param-boolean-input))
        ((string= main-tag "outputs") (throw-error "Can't make param in output."))
        ((string= main-tag "test") (insert-param-boolean-test (gather-names "boolean")))
        ((string= main-tag "tests") (progn (insert-test)
                                           (insert-param-boolean-test
                                            (gather-names "boolean"))))))

(defun insert-param-boolean-input (&optional isin-conditional label name)
  "Insert bare minimum boolean param in an input section, with WHEN parameters if IS-CONDITIONAL."
  (interactive (list (isin-conditional)
                     (read-string "Label: ")
                     (read-string "Cheetah Name: ")))
  (insert (format "<param name=\"%s\" type=\"boolean\" label=\"%s\" />" name label))
  (jump-to-start-of-tag))
;;  (param-mode-boolean))

(defun edit-param-toggle-checked (&optional point)
  (interactive "P")
  (save-excursion
    (jump-to-start-of-tag)
    (let* ((tags (get-current-subtags))
           (has-checked (member "checked" tags))
           (is-true nil)
           (just-inserted-true nil))
      (if has-checked
        (setq is-true
              (progn (re-search-forward "checked=\"\\([^\"]+\\)\"")
                     (string= "true" (match-string-no-properties 1))))
        ;; Create checked=true
        (jump-to-start-of-tag)
        (search-forward "label=")
        (backward-char 6)
        (insert "checked=\"true\" ")
        (setq just-inserted-true t))
      ;; At this point there is a checked
      (unless just-inserted-true
        (jump-to-start-of-tag)
        (search-forward "checked=")
        (let ((beg (mark))
              (end (search-forward "\" ")))
          (if is-true
              ;; Make it not checked by deleting it completely
              (delete-region (search-backward "checked") end)
            ;; Otherwise add a true
            (delete-region beg end)
            (insert "checked=\"true\"")))))))

(defun edit-param-toggle-truevalue (&optional point) )
(defun edit-param-toggle-falsevalue (&optional point) )
