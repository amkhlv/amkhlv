(provide 'bystroTeX-preview)

(require 'subr-x)
(require 'json)

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

(defun bystroTeX-unhide-at-point ()
  (setq bystroTeX--hide-region-overlays
        (seq-filter
         (lambda (ovl)
           (if
               (and
                (< (- (overlay-start ovl) 1) (point))
                (< (point) (+ (overlay-end ovl) 1)))
               (progn (delete-overlay ovl) nil)
             t))
         bystroTeX--hide-region-overlays )))
    
(defun bystroTeX--TeX-matches (tex)
  (not
   (memq
    nil ; all lines should match the given tex lines
    (mapcar
     (lambda (line)
       (and
        (search-forward line (+ (point) (length line)) t)
        (if (looking-at "[ ]*\n[ ]*") (re-search-forward "[ ]*\n[ ]*") (looking-at "}"))
        ))
     (mapcar 'string-trim (split-string tex "\n"))))))

(defvar bystroTeX--formula-insertion-in-progress nil)

(defun bystroTeX--insert-formula (tex svg)
  (setq bystroTeX--formula-insertion-in-progress t)
  (while bystroTeX--formula-insertion-in-progress
    (if (re-search-forward
         (concat bystroTeX-formula-regex "{\\([ ]*\n[ ]*\\)?")
         nil
         t)
        (let ((b nil)
              (e nil))
          (setq b (match-beginning 0))
          (when (bystroTeX--TeX-matches tex)
            (setq e (+ 1 (point)))
            (put-image (create-image svg 'svg nil :scale 1.25 :ascent 'center) (+ 1 (point)))
            (bystroTeX--make-hide-overlay b e)))
      (setq bystroTeX--formula-insertion-in-progress nil))))

(defvar bystroTeX--equation-insertion-in-progress nil)

(defun bystroTeX--insert-equation (tex svg)
  (setq bystroTeX--equation-insertion-in-progress t)
  (while bystroTeX--equation-insertion-in-progress
    (if (re-search-forward
           (concat bystroTeX-equation-regex "\\(\\[[^]]+\\]\\)?{[ ]*\n[ ]*")
           nil
           t)
        (let ((b nil)
              (e nil))
          (setq b (match-beginning 0))
          (when (bystroTeX--TeX-matches tex)
            (setq e (+ 1 (point)))
            (put-image (create-image svg 'svg nil :scale 1.4) (+ 1 (point)))
            (bystroTeX--make-hide-overlay b e)))
      (setq bystroTeX--equation-insertion-in-progress nil))))

(defun bystroTeX--get-from-sqlite ()
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (d (replace-regexp-in-string "\\.scrbl$" "/" (buffer-file-name)))
         (formulas-database (concat d "bystrotex.db")))
    (mapcar
     (lambda (x) (json-read-from-string x))
     (reverse
      (cdr
       (reverse
        (split-string
         (shell-command-to-string
          (concat "sqlite3 "
                  formulas-database
                  " \"SELECT json_object('tex', tex, 'svg', filename) FROM formulas2 ORDER BY CAST(filename AS INT);\""))
         "\n")))))))


(defun bystroTeX-preview ()
  "show preview"
  (interactive)
  (make-variable-buffer-local 'bystroTeX-preview-is-on)
  (make-variable-buffer-local 'bystroTeX--hide-region-overlays)
  (setq case-fold-search nil)
  (setq svgnum 0)
  (let* ((oldpos (point))
         (d (replace-regexp-in-string "\\.scrbl$" "/" (buffer-file-name)))
         (formulas-database (concat d "bystrotex.db"))
         )
    (defvar already-inserted '())
    (dolist
        (h (bystroTeX--get-from-sqlite))
      (let ((tex (gethash "tex" h)))
        (unless (member tex already-inserted)
          (setq already-inserted (cons tex already-inserted))
          (goto-char 0)
          (bystroTeX--insert-formula tex (concat d (gethash "svg" h) ".svg"))
          (goto-char 0)
          (bystroTeX--insert-equation tex (concat d (gethash "svg" h) ".svg"))
          )))
    (setq already-inserted '())
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

(defun bystroTeX-reveal ()
  "remove SVG under cursor"
  (interactive)
  (remove-images (- (point) 1) (+ (point) 1))
  (bystroTeX-unhide-at-point)
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




