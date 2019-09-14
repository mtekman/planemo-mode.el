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
  (when (or (string= "<" (string (char-after)))
            (not (isin-between-tags)))
    (insert "\n"))
  (jump-to-param-start)
  (execute-kbd-macro (read-kbd-macro "TAB"))
  (execute-kbd-macro (read-kbd-macro "<up>"))
  (execute-kbd-macro (read-kbd-macro "TAB"))
  (cond ((string= main-tag "inputs") (call-interactively 'insert-param-boolean-input))
        ((string= main-tag "outputs") (throw-error "Can't make param in output."))
        ((string= main-tag "test") (insert-param-boolean-test (gather-names "boolean")))
        ((string= main-tag "tests")
         (progn (insert-test)
                (insert-param-boolean-test (gather-names "boolean"))))))

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
  (edit-param-toggle "checked" "true"))


(defun edit-param-toggle-truevalue (&optional point) )
(defun edit-param-toggle-falsevalue (&optional point) )
