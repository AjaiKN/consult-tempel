;;; consult-tempel.el --- A consulting-read interface for tempel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ajai Khatri Nelson
;; Copyright (C) 2021 mohsin kaleem

;; Maintainer: Ajai Khatri Nelson <emacs@ajai.dev>
;; Author: Ajai Khatri Nelson <emacs@ajai.dev>
;; Author: mohsin kaleem <mohkale@kisara.moe>
;; Package-Requires: ((emacs "27.1") (tempel "1.11") (consult "0.16"))
;; Version: 0.2
;; URL: https://github.com/mohkale/consult-tempel

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Interactively select a tempel snippet through completing-read with in
;; buffer previews.

;; This is based on consult-yasnippet by mohsin kaleem
;; (https://github.com/mohkale/consult-yasnippet).

;;; Code:

(require 'map)
(require 'consult)
(require 'tempel)

(defgroup consult-tempel nil
  "Consult interface for tempel."
  :group 'consult
  :group 'editing)

(defcustom consult-tempel-use-thing-at-point nil
  "Use `thing-at-point' as initial value for `consult-tempel'."
  :type 'boolean
  :group 'consult-tempel)

(defcustom consult-tempel-always-overwrite-thing-at-point nil
  "Always overwrite `thing-at-point' when expanding a snippet.
This option forces `consult-tempel' to replace `thing-at-point' with the
expanded snippet even if the expansion doesn't match. This option only
applies when `consult-tempel-use-thing-at-point' is t."
  :type 'boolean
  :group 'consult-tempel)

(defun consult-tempel--expand-template (template region)
  "Expand TEMPLATE at point saving REGION."
  (deactivate-mark)
  (goto-char (car region))

  ;; Restore marked region (when it existed) so that `tempel-expand' overwrites
  ;; it.
  (when (not (string-equal "" (buffer-substring (car region) (cdr region))))
    (push-mark (point))
    (push-mark (cdr region) nil t))

  (cl-letf ((inhibit-interaction t)
            ((symbol-function 'completing-read)
             (lambda (_ _ &optional _ _ initial-input _ def &rest _)
               (or def initial-input "")))
            ((symbol-function 'completing-read-multiple)
             (lambda (_ _ &optional _ _ initial-input _ def &rest _)
               (or def initial-input "")))
            ((symbol-function 'read-number)
             (lambda (_ &optional default &rest _)
               (or default 0))))
    (tempel-insert template)))

(defun consult-tempel--bounds-of-thing-at-point (template)
  "Check for `thing-at-point' in TEMPLATE.
Returns true if `thing-at-point' is a substring of either `template-key'
or `template-name'. Matches only if `consult-tempel-use-thing-at-point'
is t."
  (if consult-tempel-use-thing-at-point
      (let* ((thing (or (thing-at-point 'symbol) ""))
             (use-thing-at-point
              (or consult-tempel-always-overwrite-thing-at-point
                  (when template
                    (equal thing (symbol-name (car template)))))))
        (if use-thing-at-point
            (or (bounds-of-thing-at-point 'symbol)
                (cons (point) (point)))
          (cons (point) (point))))
    (cons (point) (point))))

(defun consult-tempel--preview ()
  "Previewer for `consult--read'.
This function expands TEMPLATE at point in the buffer
`consult-tempel--read-template' was started in. This includes
overwriting any region that was active and removing any previous
previews that're already active.

When TEMPLATE is not given, this function essentially just resets
the state of the current buffer to before any snippets were previewed.

If `consult-tempel-use-thing-at-point' is t and region is not selected,
this function removes the matching prefix from the preview."
  (let* ((buf (current-buffer))
         (region-active-initially (use-region-p))
         (initial-region (if (use-region-p)
                             (cons (region-beginning) (region-end))
                           (cons (point) (point))))
         (initial-region-contents (buffer-substring (car initial-region) (cdr initial-region)))
         (region (cons (car initial-region) (cdr initial-region))))
    (lambda (action template)
      (with-current-buffer buf
        (let* ((inhibit-redisplay t)
               (inhibit-read-only t)
               (orig-offset (- (point-max) (cdr region))))
          ;; We always undo any snippet previews before maybe setting up
          ;; some new previews.
          (delete-region (car region) (cdr region))
          (goto-char (car region))
          (setq region (cons (car initial-region) (cdr initial-region)))
          (insert initial-region-contents)
          (when (not region-active-initially)
            (setq region (consult-tempel--bounds-of-thing-at-point template))
            (setq initial-region region)
            (setq initial-region-contents (buffer-substring (car region) (cdr region))))

          ;; Restore the region if it was initially active, so that tempel can overwrite
          (when (and region-active-initially (eq action 'return))
            (activate-mark)
            (set-mark (car region))
            (goto-char (cdr region)))

          (when (and template (not (eq action 'return)))
            (unwind-protect
                (consult-tempel--expand-template template region)
              (unwind-protect
                  nil
                  ;; (mapc #'yas--commit-snippet
                  ;;       (yas-active-snippets (point-min) (point-max)))
                (setcdr region (- (point-max) orig-offset))
                (deactivate-mark)))
            (redisplay)))))))

(defun consult-tempel--template-mode-name (template)
  (cl-loop for (mode . templates) in tempel--path-templates
           when (and (symbolp mode)
                     (memq template templates))
           return (symbol-name mode)))

(defun consult-tempel--candidates (templates)
  "Convert TEMPLATES into candidates for `completing-read'."
  (mapcar
   (lambda (template)
     (let* ((group-name (consult-tempel--template-mode-name template)))
       (cons (concat
              (propertize group-name 'consult--prefix-group group-name)
              " "
              (symbol-name (car template)))
             template)))
   templates))

(defun consult-tempel--annotate (candidates)
  (lambda (cand)
    (when-let* ((template (cdr (assoc cand candidates)))
                (mode-name (or (consult-tempel--template-mode-name template) "")))
      (concat
       " "
       (propertize " " 'display `(space :align-to (- right ,(+ 1 (length mode-name)))))
       mode-name))))

(defun consult-tempel--read-template ()
  "Backend implementation of `consult-tempel'.
This starts a `completing-read' session with all the snippets in the current
snippet table with support for previewing the snippet to be expanded and
replacing the active region with the snippet expansion.

This function doesn't actually expand the snippet, it only reads and then
returns a snippet template from the user."
  (barf-if-buffer-read-only)

  (let* ((buffer (current-buffer))
         (buffer-undo-list t) ; Prevent querying user (and showing previews) from updating the undo-history
         (buffer-read-only t)
         (candidates
          (consult-tempel--candidates
           (or (tempel--templates)
               (user-error "consult-tempel: No templates for %s" major-mode)))))
    (cl-letf (((buffer-local-value 'buffer-read-only buffer) t))
      (consult--read
       candidates
       :prompt "Choose a snippet: "
       :annotate (consult-tempel--annotate candidates)
       :initial
       (when consult-tempel-use-thing-at-point
         (thing-at-point 'symbol))
       :lookup 'consult--lookup-cdr
       :require-match t
       :state (consult-tempel--preview)
       :group 'consult--prefix-group
       :category 'tempel))))

;;;###autoload
(defun consult-tempel ()
  "Interactively select and expand a tempel template.
This command presents a completing read interface containing all currently
available snippet expansions, with live previews for each snippet. Once
selected a chosen snippet will be expanded at point using
`tempel-insert'."
  (interactive)
  (when-let* ((template (consult-tempel--read-template)))
    (when-let* (((not (region-active-p)))
                (thing-bounds (consult-tempel--bounds-of-thing-at-point template))
                (thing-start (car thing-bounds))
                (thing-end (cdr thing-bounds)))
      (push-mark thing-start 'nomsg 'activate))
    (tempel-insert template)))

(provide 'consult-tempel)
;;; consult-tempel.el ends here
