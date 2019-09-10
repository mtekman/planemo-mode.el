


;; Minor modes
(define-minor-mode galaxy-mode)

(define-minor-mode conditional-mode
  "This mode exists in galaxy-mode. Adds a <when value='' /> for every valid param value.")

(define-minor-mode param-mode
  "This mode can overlay conditional-mode")

(define-minor-mode selection-mode
  "This mode can overlay param-mode")


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
