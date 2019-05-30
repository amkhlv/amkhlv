(provide 'bystroTeX-preview)

(require 'subr-x)

(defgroup bystroTeX nil
  "Functions to preview BystroTeX .scrbl files"
  :prefix "bystroTeX-"
  :group 'convenience)

(defcustom bystroTeX-equation-regex "@\\(equation\\|e\\)"
  "Equation regex"
  :type 'string
  :group 'bystroTeX)

(defcustom bystroTeX-formula-regex "@,?f"
  "Formula regex"
  :type 'string
  :group 'bystroTeX)

(defvar bystroTeX-preview-is-on nil)

(defvar bystroTeX--hide-region-overlays nil)

(defun bystroTeX--make-hide-overlay (b e)
  (let ((new-overlay (make-overlay b e)))
    (push new-overlay bystroTeX--hide-region-overlays)
    (overlay-put new-overlay 'invisible t)
    (overlay-put new-overlay 'intangible t)))

(defun bystroTeX--unhide-all ()
  (dolist (ovl bystroTeX--hide-region-overlays)
    (delete-overlay ovl))
  (setq bystroTeX--hide-region-overlays nil))
    
(defun bystroTeX--TeX-matches (tex)
  (not
   (memq
    nil
    (mapcar
     (lambda (line)
       (and
        (search-forward line (+ 1 (point) (length line)) t)
        (if (looking-at "[ ]*\n[ ]*") (re-search-forward "[ ]*\n[ ]*") (looking-at "}"))
        ))
     (mapcar 'string-trim (split-string tex "\n"))))))

(defun bystroTeX--insert-formula (tex svg)
  (let ((b nil)
        (e nil))
    (when (re-search-forward
           (concat bystroTeX-formula-regex "{\\([ ]*\n[ ]*\\)?")
           nil
           t)
      (setq b (match-beginning 0))
      (when (bystroTeX--TeX-matches tex)
        (setq e (+ 1 (point)))
        (put-image (create-image svg) (+ 1 (point)))
        (bystroTeX--make-hide-overlay b e))
      (bystroTeX--insert-formula tex svg))))

(defun bystroTeX--insert-equation (tex svg)
  (let ((b nil)
        (e nil))
    (when (re-search-forward
           (concat bystroTeX-equation-regex "\\(\\[[^]]+\\]\\)?{[ ]*\n[ ]*")
           nil
           t)
      (setq b (match-beginning 0))
      (when (bystroTeX--TeX-matches tex)
        (setq e (+ 1 (point)))
        (put-image (create-image svg) (+ 1 (point)))
        (bystroTeX--make-hide-overlay b e))
      (bystroTeX--insert-equation tex svg))))

(defun bystroTeX-preview ()
  "show preview"
  (interactive)
  (make-variable-buffer-local 'bystroTeX-preview-is-on)
  (make-variable-buffer-local 'bystroTeX--hide-region-overlays)
  (setq case-fold-search nil)
  (setq svgnum 0)
  (let* ((oldpos (point))
         (d (replace-regexp-in-string "\\.scrbl$" "/" (buffer-file-name)))
         (formulas-database (concat d "formulas.sqlite"))
         )
    (dolist
        (texstr
         (reverse
          (cdr
           (reverse
            (split-string
             (shell-command-to-string
              (concat "sqlite3"
                      " -separator"
                      " '"
                      "\n"
                      "' "
                      formulas-database
                      " \"select tex, '' from formulas order by CAST(filename AS INT);\"")
              )
             "\n\n"
             )))))
      (setq svgnum (1+ svgnum))
      (goto-char 0)
      (bystroTeX--insert-formula texstr (concat d (number-to-string svgnum) ".svg"))
      (goto-char 0)
      (bystroTeX--insert-equation texstr (concat d (number-to-string svgnum) ".svg"))
      )
    (setq bystroTeX-preview-is-on t)
    (goto-char oldpos)
    )
  )

(defun bystroTeX-clean ()
  "remove SVGs and show TeX"
  (interactive)
  (make-variable-buffer-local 'bystroTeX-preview-is-on)
  (make-variable-buffer-local 'bystroTeX--hide-region-overlays)
  (remove-images (point-min) (point-max))
  (setq bystroTeX-preview-is-on nil)
  (bystroTeX--unhide-all)
  )

(defun bystroTeX-toggle-preview ()
  "toggle preview"
  (interactive)
  (if bystroTeX-preview-is-on (bystroTeX-clean) (bystroTeX-preview)))

(defun bystroTeX-toggle-preview-and-recenter ()
  "toggle preview"
  (interactive)
  (bystroTeX-toggle-preview)
  (recenter)
  )

;; Searching for exact string
;; ==========================
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/String-Search.html

;; Hiding formula text
;; ===================
;; https://stackoverflow.com/questions/12258334/how-to-use-hide-region-package-in-elisp




