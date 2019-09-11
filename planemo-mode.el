;;

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
  '(([M-c] . insert-conditional)
    ([M-s] . insert-section)
    ([M-p] . insert-param)
    ([M-d] . insert-data)
    ([M-t] . insert-test)))

(define-minor-mode conditional-mode
  "This mode exists in galaxy-mode. Adds a <when value='' /> for every valid param value."
  :init-value nil
  :lighter "Conditional "
  :keymap
  '(([M-p] . insert-param)
    ([M-w] . insert-when)))

(define-minor-mode param-selection-mode
  "This mode can overlay param-mode"
  :init-value nil
  :lighter "Selection "
  :keymap
  '(([M-S-o] . insert-option-nowhen)
    ([M-o] . insert-option)))

(define-minor-mode param-mode
  "This mode can overlay conditional-mode"
  :init-value nil
  :lighter "Param "
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t b") 'insert-param-boolean)
    (define-key map (kbd "C-t i") 'insert-param-integer)
    (define-key map (kbd "C-t f") 'insert-param-float)
    (define-key map (kbd "C-t t") 'insert-param-text)
    (define-key map (kbd "C-t d") 'insert-param-data)
    (define-key map (kbd "C-t s") 'insert-param-selection)
    map))

b
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
