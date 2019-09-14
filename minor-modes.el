;; Minor modes
(define-minor-mode galaxy-mode
  "Main galaxy mode"
  :init-value nil
  :lighter "Galaxy "
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t g c") 'insert-conditional)
    (define-key map (kbd "C-t g s") 'insert-section)
    (define-key map (kbd "C-t g d") 'insert-data)
    (define-key map (kbd "C-t g t") 'insert-test)
    (define-key map (kbd "C-t p p") 'insert-param)
    (define-key map (kbd "C-t p b") 'insert-param-boolean)
    (define-key map (kbd "C-t p i") 'insert-param-integer)
    (define-key map (kbd "C-t p f") 'insert-param-float)
    (define-key map (kbd "C-t p t") 'insert-param-text)
    (define-key map (kbd "C-t p d") 'insert-param-data)
    (define-key map (kbd "C-t p s") 'insert-param-selection)
   map))

(define-minor-mode conditional-mode
  "This mode nests the param-mode"
  :init-value nil
  :lighter "Conditional "
  :keymap
  (let ((map (make-sparse-keymap)))
    ;; Modes below inherit these
    (define-key map (kbd "C-t v") 'insert-when)
    (define-key map (kbd "C-t d") 'delete-when)
    map))

(define-minor-mode param-mode
  "This mode nests the other param-mode-*"
  :init-value nil
  :lighter "Param "
  :keymap
  (let ((map (make-sparse-keymap)))
    ;; Modes below inherit these
    (define-key map (kbd "C-t q") 'quit-mode)
    (define-key map (kbd "C-t o") 'edit-param-toggle-optional)
    (define-key map (kbd "C-t h") 'edit-param-help)
    (define-key map (kbd "C-t a") 'edit-param-argument)
    (define-key map (kbd "C-t n") 'edit-param-name)
    (define-key map (kbd "C-t l") 'edit-param-label)
    map))

(define-minor-mode sanitizer-mode)

(define-minor-mode param-mode-data
  "Overlays param-mode"
  :init-value nil
  :lighter "data "
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t d v") 'edit-param-value)
    (define-key map (kbd "C-t d f") 'edit-param-format)
    map))

(define-minor-mode param-mode-text
  "Overlays param-mode"
  :init-value nil
  :lighter "text "
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t t v") 'edit-param-value)
    (define-key map (kbd "C-t t s") 'edit-or-insert-sanitizer)
    map))

(define-minor-mode param-mode-numeric
  "This mode can overlay conditional-mode"
  :init-value nil
  :lighter "numeric "
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t n t") 'edit-param-numeric-toggle-type)
    (define-key map (kbd "C-t n m") 'edit-param-min)
    (define-key map (kbd "C-t n x") 'edit-param-max)
    (define-key map (kbd "C-t n v") 'edit-param-value)
    map))


(define-minor-mode param-mode-selection
  "This mode can overlay conditional-mode"
  :init-value nil
  :lighter "selection "
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t s m") 'edit-param-selection-toggle-multiple)
    (define-key map (kbd "C-t s o") 'insert-option) ;; adds when via prompt if conditional detected
    (define-key map (kbd "C-t s d") 'delete-option) ;; if when not empty, leaves it and blanks out name
    map))
