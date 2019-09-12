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

(defun insert-param (&optional type)
  "Checks whether it is in an input or output parent.
Checks whether the conditional-mode is active, and if so prompts to insert when"
  (interactive
   (list (completing-read "Type: " '("boolean" "data" "integer" "float" "text" "selection"))))
  (funcall (intern (concat "insert-param-" type))))

(defun find-valid-parent (&optional point)
  "Jumps up an XML from a point to find a valid parent tag."
  (interactive "d")
  (let ((parent-tag nil))
    (save-excursion
      (while (not (member parent-tag '("inputs" "tests" "test" "outputs")))
        (nxml-up-element)
        (nxml-backward-element)
        (let ((beg (+ 1 (point)))
              (end (progn (re-search-forward "<\\([^> ]+\\)")
                          (match-end 0))))
          (setq parent-tag (buffer-substring-no-properties beg end))))
      parent-tag)))

(defun insert-param-boolean-input (is-conditional &optional name label &rest optional checked help truevalue falsevalue)
  "Insert bare minimum boolean param in an input section, with WHEN parameters if IS-CONDITIONAL."
  (interactive (list (read-string "name: ")
                     (read-string "label: ")))
  (insert "<param name=\"%s\" type=\"boolean\" label=\"%s\" />")
  (search-backward "<param")
  (param



(defun insert-param-boolean (&optional main-tag)
  (interactive (list (find-valid-parent)))
  (cond ((string= main-tag "inputs") (insert-param-boolean-input))
        ((string= main-tag "outputs") (throw-error "Can't make param in output."))
        ((string= main-tag "test") (insert-param-boolean-test (gather-names "boolean")))
        ((string= main-tag "tests") (progn (insert-test)
                                           (insert-param-boolean-test
                                            (gather-names "boolean"))))))



(defun my-test (&optional tmp)
  (interactive
   (list (completing-read "Type: " '("boolean" "data" "integer" "float" "text" "selection"))))
  (if tmp
      (message (concat "WORD " tmp))
    (message "NO ARG")))

(defun insert-section ())
(defun insert-when ())

;; Minor modes
(define-minor-mode galaxy-mode
  "Main galaxy mode"
  :init-value nil
  :lighter "Galaxy "
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t b") 'insert-conditional)
    (define-key map (kbd "C-t b") 'insert-section)
    (define-key map (kbd "C-t b") 'insert-data)
    (define-key map (kbd "C-t b") 'insert-test)))
    (define-key map (kbd "C-t b") 'insert-param)
    (define-key map (kbd "C-t b") 'insert-param-boolean)
    (define-key map (kbd "C-t i") 'insert-param-integer)
    (define-key map (kbd "C-t f") 'insert-param-float)
    (define-key map (kbd "C-t t") 'insert-param-text)
    (define-key map (kbd "C-t d") 'insert-param-data)
    (define-key map (kbd "C-t s") 'insert-param-selection)



(define-minor-mode conditional-mode
  "This mode nests the param-mode"
  :init-value nil
  :lighter "Conditional "
  :keymap
  (let ((map (make-sparse-keymap)))
    ;; Modes below inherit these
    (define-key map (kbd "v") 'insert-when)
    (define-key map (kbd "d") 'delete-when)))
  

(define-minor-mode param-mode
  "This mode nests the other param-mode-*"
  :init-value nil
  :lighter "Param "
  :keymap
  (let ((map (make-sparse-keymap)))
    ;; Modes below inherit these
    (define-key map (kbd "q") 'quit-mode)
    (define-key map (kbd "o") 'edit-param-toggle-optional)
    (define-key map (kbd "h") 'edit-param-help)
    (define-key map (kbd "a") 'edit-param-argument)
    (define-key map (kbd "n") 'edit-param-name)
    (define-key map (kbd "l") 'edit-param-label)
    map))

(define-minor-mode sanitizer-mode)

(define-minor-mode param-mode-data)

(define-minor-mode param-mode-text
  "Overlays param-mode"
  :init-value nil
  :lighter "text "
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t v") 'edit-param-value)
    (define-key map (kbd "t s") 'edit-or-insert-sanitizer)
    map))

(define-minor-mode param-mode-numeric
  "This mode can overlay conditional-mode"
  :init-value nil
  :lighter "numeric "
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n t") 'edit-param-numeric-toggle-type)
    (define-key map (kbd "n m") 'edit-param-min)
    (define-key map (kbd "n x") 'edit-param-max)
    (define-key map (kbd "n v") 'edit-param-value)
    map))


(define-minor-mode param-mode-selection
  "This mode can overlay conditional-mode"
  :init-value nil
  :lighter "selection "
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s o") 'insert-option) ;; adds when via prompt if conditional detected
    (define-key map (kbd "s d") 'delete-option) ;; if when not empty, leaves it and blanks out name
    map))

(define-minor-mode param-mode-boolean
  "This mode can overlay conditional-mode"
  :init-value nil
  :lighter "boolean "
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b c") 'edit-param-toggle-checked)
    (define-key map (kbd "b t") 'edit-param-truevalue)
    (define-key map (kbd "b f") 'edit-param-falsevalue)
    map))


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
