;; rte related commands start

(defun edts-rte-run ()
  (interactive)
  (let* ((module         (car   (find-mfa-under-point)))
         (fun            (cadr  (find-mfa-under-point)))
         (arity          (caddr (find-mfa-under-point))))
    (if (get-buffer (param-buffer))
          (let* ((mfa            (parse-mfa))
                 (module-buffer  (car mfa))
                 (node           (edts-buffer-node-name))
                 (fun-buffer     (cadr mfa))
                 (args-buffer    (caddr mfa)))
            (if (and (string-equal module module-buffer)
                     (string-equal (fun-arity-str fun arity) fun-buffer))
                (edts-rte-run-with-args args-buffer))))))

(defun edts-rte-run-with-args (arguments)
  "Run on function using rte_run"
  (interactive "sInput Arguments:")
  (let* ((node       (edts-buffer-node-name))
         (resource   (list "plugins" "rte" node "cmd"))
         (args       nil)
         (module     (car (find-mfa-under-point)))
         (fun        (cadr (find-mfa-under-point)))
         (body       (get_rte_run_body module fun arguments)))
    (ensure-args-saved arguments)
    (edts-rest-post resource args body)
    ))

(defun param-buffer ()
  "Return the name of the parameter buffer for the current node"
  (let* ((node (edts-buffer-node-name)))
    (concat "*" node "-" "params" "*")
    ))

(defun ensure-args-saved (args)
  "Ensure that the module function and arguments are saved in
   the parameter buffer"
  (let* ((module         (car   (find-mfa-under-point)))
         (fun            (cadr  (find-mfa-under-point)))
         (arity          (caddr (find-mfa-under-point))))
    (save-excursion
      (set-buffer (get-buffer-create (param-buffer)))
      (erase-buffer)
      (insert (concat "module: " module                    "\n"
                      "fun: "    (fun-arity-str fun arity) "\n"
                      "args: "   args                      "\n"
                      )))))

(defun fun-arity-str (fun arity)
  "Return the func/arity string"
  (concat fun "/" (number-to-string arity)))

(defun parse-mfa ()
  "Parse the module function args"
  (save-excursion
    (set-buffer (param-buffer))
    (let* ((mfa    (split-string (trim-string (buffer-string)) "\n"))
           (module (trim-string (car mfa)))
           (fun    (trim-string (cadr mfa)))
           (args   (trim-string (caddr mfa))))
      (mapcar (lambda (str)
                (print str)
                (trim-string
                 (cadr
                  (split-string (trim-string str) ":"))))
              (list module fun args)))))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
(replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))
)

(defun get_rte_run_body(module function args)
  "Get the json body for rte_run rest request"
  (format "{\"cmd\": \"rte_run\",\"args\": [\"%s\",\"%s\", \"%s\"]}" module function args)
  )

;; find the mfa of the point
(defun find-mfa-under-point ()
  "find the mfa in which the current cursor is located."
  (interactive)
  (save-excursion
    (ferl-beginning-of-function)
    (edts-mfa-at))
  )

(defun edts-display-erl-fun-in-emacs (string buffer)
  "display a piece of erlang code in a buffer"
  (window-normalize-buffer-to-switch-to buffer)
  (display-buffer buffer)
  (with-current-buffer buffer
    (save-excursion
      (erase-buffer)
      (goto-char (point-max))
      (insert string)
      (erlang-mode)
      (edts-rte-mode))))

;; rte related commands end

;;;###autoload
(define-minor-mode edts-rte-mode
  "Display the replaced value returned by edts-rte.
When edts-rte replaces a variable with a value, a tuple in the format
of {\"__edts-rte__\", VarName, Value} is returned. Value should be displayed
and VarName should be displayed in the pop up window when the cursor is on
top of it"
  :init-value nil
  :lighter "-EDTS-RTE"
  (cond (edts-rte-mode
         (highlight-rte-vars)
         (replace-rte-vars)
         (activate-advice)
         (font-lock-fontify-buffer))
        (t
         (font-lock-remove-keywords
          nil (rte-keywords))
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward (rte-regex) nil t)
             (funcall (switch-invisible) nil)))
         (deactivate-advice))))

(defun highlight-rte-vars ()
  (interactive)
  "Highlight the tuple {\"__edts-rte__\", VarName, Value} returned by edts rte"
  (font-lock-add-keywords
   nil (rte-keywords)))

(defun replace-rte-vars ()
  "Replace the tuple {\"__edts-rte__\", VarName, Value} returned by edts rte
with Value"
  (interactive)
  (save-excursion
      (goto-line (point-min))
      (while (re-search-forward (rte-regex) nil t)
        (funcall (switch-invisible) t))))

(defun display-rte-var ()
  "Display the variable name in the tuple {\"__edts-rte__\", VarName, Value}
returned by edts rte"
  (interactive)
  (let* ((cur-point   (point))
         (displayed-p nil))
    (save-excursion
      (goto-line (point-min))
      (while (and (not displayed-p)
                  (re-search-forward (rte-regex) nil t))
        (if (and (>= cur-point (match-beginning 0))
                 (<= cur-point (match-end 0)))
            (progn (put-text-property (match-beginning 1) (match-end 1) 'invisible nil)
                   (message (concat "Variable Name: "
                                    (buffer-substring (match-beginning 1) (match-end 1))))
                   (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
                   (setq displayed-p t))
          (message ""))))))

;; question mark means non-greedy
(defun rte-regex ()
  "Regex to match the return replaced vars from the edts-rte"
  "\{b,\\(.+?\\),s,\\(.+?\\)\,e}"
  )

(defadvice forward-char (after forward-display-rte-var)
  "Advice for forward-char for displaying the rte variable name"
  (display-rte-var))

(defadvice backward-char (after backward-display-rte-var)
  "Advice for backward-char for displaying the rte variable name"
  (display-rte-var))

(defadvice next-line (after next-line-display-rte-var)
  "Advice for next-line for displaying the rte variable name"
  (display-rte-var))

(defadvice previous-line (after previous-line-display-rte-var)
  "Advice for previous for displaying the rte variable name"
  (display-rte-var))

(defun activate-advice ()
  (interactive)
  "Activate all the advices"
  (mapcar (lambda (advice) (ad-activate advice)) (rte-advices)))

(defun deactivate-advice ()
  (interactive)
  "Deactivate all the advices"
  (mapcar (lambda (advice) (ad-deactivate advice)) (rte-advices)))

(defun rte-advices ()
  "All the advices defined in edts rte mode"
  '(forward-char backward-char next-line previous-line))

(defun switch-invisible ()
  "Return a function to switch the invisible property for the part of the
value returned by rte."
  (lambda (flag)
    (put-text-property (match-beginning 0) (match-beginning 2) 'invisible flag)
    (put-text-property (match-end 2) (match-end 0) 'invisible flag)))

(defun rte-keywords ()
  "Keywords in edts-rte mode that needs to be added to font lock"
  `((,(rte-regex) 0 'font-lock-warning-face prepend)
    ("^\\(\\.+\\)" 0 'highlight prepend)))

(add-hook 'edts-code-after-compile-hook 'edts-rte)

(defun edts-rte (result)
  "Execute the edts-rte-run function when there is no error in
the file"
  (when (not (eq result 'error))
    (edts-rte-run)))

(provide 'edts-rte)
