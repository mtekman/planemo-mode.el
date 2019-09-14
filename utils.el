(require 'subr-x)

(defvar starting-tags-regex "<\\([^/> ]+\\)" "Matches the start of a tag")

(defun isin-between-tags (&optional point)
  "Whether current POINT is in between tags"
  (save-excursion
    (let* ((cur (point))
           (beg (+ (re-search-backward ">\s*") 1))
           (end (- (re-search-forward "\s*<") 1))
           (text (concat
                  (buffer-substring-no-properties beg cur)
                  (buffer-substring-no-properties cur end)))
           (stex (string-trim text)))
      (< (length stex) 1))))

(defun isin-conditional (&optional point)
  "Current point has direct parent who is a conditional."
  (interactive "d")
  (let ((tags-found nil) (parent-tag nil))
    (save-excursion
      (while (not (member parent-tag '("when" "conditional")))
        (jump-to-param-start)
        (nxml-backward-up-element)
        (let ((beg (+ 1 (point)))
              (end (progn (re-search-forward starting-tags-regex)
                          (match-end 0))))
          (setq parent-tag (buffer-substring-no-properties beg end))
          (unless (string= parent-tag "param")
            (cl-pushnew parent-tag tags-found)))))
    (string= "conditional" (car (last tags-found)))))

(defun find-valid-parent (&optional point)
  "Jumps up an XML from a point to find a valid parent tag."
  (interactive "d")
  (let ((parent-tag nil))
    (save-excursion
      (while (not (member parent-tag '("inputs" "tests" "test" "outputs")))
        (nxml-backward-up-element)
        (let ((beg (+ 1 (point)))
              (end (save-excursion
                     (re-search-forward starting-tags-regex)
                     (match-end 0))))
          (setq parent-tag (buffer-substring-no-properties beg end))))
      parent-tag)))

(defun jump-to-start-of-tag ()
  "Moves cursor to start of any tag element."
  (save-current-buffer
    ; Saving needed, otherwise jumping doesn't work on loose buffer.
    (let ((charbefore (string (char-before)))
          (charfirst (string (char-after)))
          (charsecond (string (char-after (+ 1 (point))))))
      (when (and (string= charfirst "<") (not (string= charsecond "/")))
        ;; The very first "<param" - if (nxml-up-element) is called here
        ;; it jumps out of the section.
        (forward-char 2))
      (when (string= charbefore ">")
        ;; If right at the end of the tag, (nxml-up-element) also jumps out.
        (backward-char 2))
      (nxml-up-element)
      (nxml-backward-element))))

(defun jump-to-param-start ()
  (jump-to-start-of-tag)
  (let* ((beg (point))
         (end (search-forward " "))
         (text (buffer-substring-no-properties
                (+ 1 beg) (- end 1))))
    (cond ((string= text "param") (search-backward "<param "))
          ((string= text "option") (search-backward "<param "))
          (t (progn (search-forward "<param ")
                    (search-backward "<"))))))

(defun jump-to-param-end () (jump-to-param-start)(nxml-forward-element))

(defun get-current-subtags (&optional point)
  "Returns all subtags within a tag at point"
  (interactive "P")
  (save-excursion
    (jump-to-start-of-tag)
    (let ((beg (point))
          (end (search-forward ">"))
          (revtags nil))
      (goto-char beg)
      (while (re-search-forward "\\([a-z]+\\)=\"[^\"]+\"" end revtags)
        (cl-pushnew (match-string-no-properties 1) revtags))
      revtags)))
