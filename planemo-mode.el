;;

(defun insert-boolean ())
(defun insert-data ())
(defun insert-float ())
(defun insert-input ())
(defun insert-integer ())
(defun insert-option ())
(defun insert-option-nowhen ())
(defun insert-output ())
(defun insert-selection ())
(defun insert-test ())
(defun insert-test-interactive ())
(defun insert-text ())
(defun insert-conditional ())
(defun insert-param ())
(defun insert-section ())
(defun insert-when ())

;; Minor modes
(define-minor-mode galaxy-mode
  "Main galaxy mode"
  :init-value nil
  :lighter "Galaxy "
  :keymap
  ;; Repeatable elements get modes
  '(([M-i] . insert-input)
    ([M-o] . insert-output)
    ([M-t] . insert-test)))

(define-minor-mode tests-mode
  "Output mode"
  :init-value nil
  :lighter "Tests "
  :keymap
  '(([M-t] . insert-test)
    ([M-i] . insert-test-interactive)))

(define-minor-mode outputs-mode
  "Output mode"
  :init-value nil
  :lighter "Output "
  :keymap
  '(([M-d] . insert-data)))

(define-minor-mode inputs-mode
  "Input mode"
  :init-value nil
  :lighter "Input "
  :keymap
  '(([M-c] . insert-conditional)
    ([M-p] . insert-param)
    ([M-s] . insert-section))

(define-minor-mode conditional-mode
  "This mode exists in galaxy-mode. Adds a <when value='' /> for every valid param value."
  :init-value nil
  :lighter "Conditional "
  :keymap
  '(([M-p] . insert-param)
    ([M-w] . insert-when)))

(define-minor-mode param-mode
  "This mode can overlay conditional-mode"
  :init-value nil
  :lighter "Param "
  :keymap
  '(([M-b] . insert-boolean)
    ([M-i] . insert-integer)
    ([M-f] . insert-float)
    ([M-t] . insert-text)
    ([M-d] . insert-data)
    ([M-s] . insert-selection)))

(define-minor-mode param-selection-mode
  "This mode can overlay param-mode"
  :init-value nil
  :lighter "Selection "
  :keymap
  '(([M-O] . insert-option-nowhen)
    ([M-o] . insert-option)))



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
