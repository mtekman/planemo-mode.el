
(defun insert-param-boolean ())
(defun insert-param-data ())
(defun insert-param-float ())
(defun insert-param-integer ())
(defun insert-param-text ())
(defun insert-param-selection ())
(defun insert-option ())
(defun insert-option-nowhen ())
(defun insert-output ())
(defun insert-selection ())
(defun insert-test ())
(defun insert-test-interactive ())
(defun insert-conditional ())

(defun insert-param (&optional type)
  "Checks whether it is in an input or output parent. Checks whether the conditional-mode is active, and if so prompts to insert when."
  (interactive
   (list (completing-read "Type: " '("boolean" "data" "integer" "float" "text" "selection"))))
  (funcall (intern (concat "insert-param-" type))))


(defun insert-section ())
(defun insert-when ())

;; Commands
(defun macrofy-selection (macroname beg end)
  "Convert selection to a macro and replace selection with expand token. Adds to the <macro/> section. "
  (interactive
   (setq macroname (read-string "macro name (without @) "))))

(defun tokenize-selection (tokenname beg end)
  "Convert selection to a token and replace selection with token name surrounded by @. Adds to the <macro/> section. It then jumps through buffer at each value match, asking to replace the match with the token name")

(defun checkormake-tag-galaxy ()
  "Validate the buffer. Should be able to expand macros (i.e. it expands all into a temp buffer and checks that)."
  (interactive)
  (checkormake-tag-tool)
  (checkormake-tag-description)
  (checkormake-tag-macros)
  (checkormake-tag-requirements)
  (checkormake-tag-command-and-version)
  (checkormake-tag-inputs)
  (checkormake-tag-outputs)
  (checkormake-tag-tests)
  (checkormake-tag-outputs)
  (checkormake-tag-help))
