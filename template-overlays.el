(require 'ov)

;;(require 'async-await)

(defun ov-regexp-replace (regexp replace &optional beg end)
  "Make overlays spanning the regions that match REGEXP.
REPLACE should be a function that is called to replace the matched REGEXP.
If BEG and END are numbers, they specify the bounds of the search."
  (save-excursion
    (goto-char (or beg (point-min)))
    (let (ov-or-ovs finish)
      (ov-recenter (point-max))
      (while (and (not finish) (re-search-forward regexp end t))
        ;; Apply only when there are not overlays already
        (when (not (overlays-at (match-beginning 0)))
          (let ((ov (ov-make (match-beginning 0)
                             (match-end 0)
                             nil (not ov-sticky-front) ov-sticky-rear)))
            
            ;; (ov-set ov 'display (funcall replace
            ;;                              (buffer-substring
            ;;                                       (match-beginning 0)
            ;;                                       (match-end 0))))
            (let ((replacement (funcall replace
                                    (buffer-substring-no-properties
                                     (match-beginning 0)
                                     (match-end 0)))))
              (overlay-put ov 'display replacement))
            (setq ov-or-ovs (cons ov ov-or-ovs))))
        (when (= (match-beginning 0) (match-end 0))
          (if (eobp)
              (setq finish t)
            (forward-char 1))))
      ov-or-ovs)))

(defun tov/set-overlays (&optional force)
  (ov-set (ov-regexp-replace "{%\s*\\(.*?\\)\s*%}"
                             (lambda (match)
                               (let ((content (buffer-substring-no-properties (match-beginning 1)
                                                                              (match-end 1))))
                                 content)))
          'face '(;;:underline t
                             :box t
                             :foreground "green"
                             ;;:background "green"
                             :weight :bold))

  (ov-set (ov-regexp-replace "{{\s*\\(.*?\\)\s*}}"
                             (lambda (match)
                               (let ((content (buffer-substring-no-properties (match-beginning 1)
                                                                              (match-end 1))))
                                 content)))
          'face '(;;:underline t
                             :box t
                             :foreground "green"
                             :weight :bold))
  t)


(defun overlays-at-point ()
  (overlays-at (point)))

(defun delete-overlays-at-point ()
  (mapcar 'delete-overlay (overlays-at (point))))

(defun tov/update-overlays ()
  (let ((commands (list 'outshine-self-insert-command
                        'backward-delete-char-untabify
                        'backward-delete-char
                        'delete-forward-char)))
    (message (prin1-to-string this-command))
    (unless (or (member this-command commands)
                (equal (point) last-post-command-position))
      (let ((my-current-word (thing-at-point 'word)))
        (incf times)
        ;;(message "%s" my-current-word)
        (delete-overlays-at-point)
        
        ;; (when (and (zerop (mod times wait-times))
        ;;            (not running)
        ;;            (not (equal my-current-word last-current-word)))
        ;;   (promise-new (lambda (resolve _reject)
        ;;                  (setq running t)
        ;;                  (message "Running")
        ;;                  (set-overlays)
        ;;                  (setq last-current-word my-current-word)
        ;;                  (setq running nil)
        ;;                  (funcall resolve t))))

        (set-overlays)
        (delete-overlays-at-point)
        ))
    (setq last-post-command-position (point))))

(define-minor-mode template-overlays-mode
  "Template overlays minor mode"
  :lighter " tov"
  (require 'ov) 
  (message "Template overlays is %s" (if template-overlays-mode "on" "off"))
  ;; (and template-overlays-mode
  ;;      (eldoc-mode 1)
  ;;      (eldoc-current-symbol)
  ;;      (my-contextual-help :force))

  (make-variable-buffer-local 'last-post-command-position)
  (make-variable-buffer-local 'last-current-word)
  (make-variable-buffer-local 'running)
  (make-variable-buffer-local 'wait-times)
  (make-variable-buffer-local 'times)

  (setq wait-times 5)
  (setq times 0)
  (add-to-list 'post-command-hook #'tov/update-overlays)
  
  )

(provide 'template-overlays)
