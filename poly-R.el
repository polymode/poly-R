;;; poly-R.el --- Various polymodes for R language -*- lexical-binding: t -*-
;;
;; Author: Vitalie Spinu
;; Maintainer: Vitalie Spinu
;; Copyright (C) 2013-2018 Vitalie Spinu
;; Version: 0.1.5
;; Package-Requires: ((emacs "25") (polymode "0.1.5") (poly-markdown "0.1.5") (poly-noweb "0.1.5"))
;; URL: https://github.com/polymode/poly-R
;; Keywords: languages, multi-modes
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'polymode)
(require 'ess-mode)
(require 'ess-r-mode nil t)

(defgroup poly-R nil
  "Settings for poly-R polymodes"
  :group 'polymode)

(defcustom pm-poly/R
  (pm-polymode :name "R"
               :hostmode 'pm-host/R
               :innermodes '(pm-inner/fundamental))
  "R root polymode intended to be inherited from."
  :group 'polymodes
  :type 'object)


;; NOWEB
(require 'poly-noweb)

(defcustom pm-inner/noweb-R
  (clone pm-inner/noweb
         :name "noweb-R"
         :mode 'R-mode)
  "Noweb for R"
  :group 'poly-innermodes
  :type 'object)

;;;###autoload (autoload 'poly-noweb+R-mode "poly-R")
(define-polymode poly-noweb+R-mode pm-poly/noweb
  :lighter " PM-Rnw"
  :innermodes '(pm-inner/noweb-R))

;;;###autoload
(defalias 'poly-noweb+r-mode 'poly-noweb+R-mode)


;; MARKDOWN
(require 'poly-markdown)

(defcustom poly-r-rmarkdown-template-dirs nil
  "Directories containing RMarkdown templates.
Templates are either folders in rmarkdown template format or
simple Rmd files. First level nesting is allowed in the sense
that each sub-directory of these directories can itself contain
templates."
  :type '(repeat string)
  :group 'poly-R)

;;;###autoload (autoload 'poly-markdown+R-mode "poly-R")
(define-polymode poly-markdown+R-mode poly-markdown-mode :lighter " PM-Rmd")

;;;###autoload
(defalias 'poly-markdown+r-mode 'poly-markdown+R-mode)

(defun poly-r-rmarkdown-templates (&optional proc)
  (let* ((ess-dialect "R")
         (proc (or proc (ess-get-next-available-process "R" t)))
         (user-dirs (if poly-r-rmarkdown-template-dirs
                        (format "c(\"%s\")"
                                (mapconcat #'identity
                                           poly-r-rmarkdown-template-dirs
                                           "\", \""))
                      "c()"))
         (cmd (concat "
local({
list_templates <- 
 function(user_dirs) {
    user_dirs <- c(user_dirs, unlist(lapply(user_dirs, list.dirs, recursive = FALSE)))
    list_from_dir <- function(dir, pkg = NULL) {
      pkg_name <- if(is.null(pkg)) 'USER' else pkg
      if (dir.exists(dir)) {
        dirs <- list.dirs(dir, recursive = FALSE)
        for (d in dirs) {
          yaml_file <- file.path(d, 'template.yaml')
          if (file.exists(yaml_file)) {
            yaml <- rmarkdown:::yaml_load_file_utf8(yaml_file)
            if (is.null(desc <- yaml$description)) desc <- 'nil'
            if (!is.null(pkg)) d <- basename(d)
            cat(sprintf('(\"%s\" \"%s\" \"%s\" \"%s\")\n', pkg_name, d, yaml$name, desc))
          }
        }
      }
    }
    cat('(\n')
    for (user_dir in user_dirs) {
      files <- list.files(user_dir,  '\\\\.[Rr]md$', full.names = TRUE)
      for (f in files) {
        name <- sub('\\\\.[^.]+$','', basename(f))
        cat(sprintf('(\"USER\" \"%s\" \"%s\")\n', f, name))
      }
      list_from_dir(user_dir)
    }
    packages <- sort(unlist(lapply(.libPaths(), dir)))
    for (pkg in packages) {
      template_folder <- system.file('rmarkdown', 'templates', package = pkg)
      list_from_dir(template_folder, pkg)
    }
    cat(')\n')
  }\n
"
                      (format "list_templates(%s)})\n" user-dirs))))
    (with-current-buffer (ess-command cmd nil nil nil nil proc)
      (goto-char (point-min))
      (if (save-excursion (re-search-forward "\\+ Error" nil t))
          (error "%s" (buffer-string))
        (when (re-search-forward "(" nil t)
          (backward-char)
          (read (current-buffer)))))))

(defun poly-r-rmarkdown-templates-menu (&optional _items)
  (let* ((proc (ess-get-next-available-process "R" t))
         (templates (process-get proc :rmarkdown-templates)))
    (unless templates
      (setq templates (poly-r-rmarkdown-templates proc))
      (when (< 2000 (length templates))
        (process-put proc :rmarkdown-templates templates)))
    (mapcar (lambda (el)
              (cons (car el) (mapcar (lambda (tlate)
                                       (vector (nth 2 tlate)
                                               `(poly-r--rmarkdown-draft ,(nth 0 tlate) ,(nth 1 tlate))
                                               :help (nth 3 tlate)))
                                     (cdr el))))
            (seq-group-by #'car templates))))

(defun poly-r--rmarkdown-draft (pkg template)
  (let* ((ess-dialect "R")
         (file (read-file-name "Create from template: "))
         (pkg (if (string= pkg "USER") "NULL" (format "\"%s\"" pkg)))
         (cmd (format "rmarkdown::draft('%s', '%s', package = %s, edit = FALSE)" file template pkg)))
    (if (and (string= pkg "NULL")
             (file-exists-p template)
             (not (file-directory-p template)))
        (copy-file template file t)
      (when (file-exists-p file)
        (if (y-or-n-p (format "File '%s' already exists.  Overwrite? " file))
            (delete-file file)
          (signal 'quit t)))
      (ess-eval-linewise cmd)
      (sit-for 0.05)
      (let ((output (car (ess-get-words-from-vector "print(.Last.value)\n"))))
        (when output
          (setq file output))))
    (if (file-exists-p file)
        (find-file-other-window file)
      (error "No file '%s' created" file))))

(defvar poly-r--rmarkdown-create-from-template-hist nil)
(defun poly-r-rmarkdown-create-from-template ()
  "Create new Rmarkdown project from template.
Templates are provided by R packages in the format described in
the help of 'draft' function and chapter 17 of the RMarkdown
book. This function also searches for templates in
`poly-r-rmarkdown-template-dirs' directories. Templates in those
directories are either simple Rmd files or template directories
with template.yaml metadata as enforced by rmarkdwon template
format.

Common packages containing templates are rticles, tufte,
xaringan, prettydoc, revealjs, flexdashboards.  Some more
templates at:

 https://github.com/mikey-harper/example-rmd-templates
 https://github.com/hrbrmstr/markdowntemplates
 https://github.com/svmiller/svm-r-markdown-templates
"
  (interactive)
  (let* ((specs (poly-r-rmarkdown-templates))
         (spec (cdr (pm--completing-read "Template: "
                                         (mapcar (lambda (el) (cons (format "%s/%s" (car el) (nth 2 el)) el))
                                                 specs)
                                         nil t nil 'poly-r--rmarkdown-create-from-template-hist))))
    (poly-r--rmarkdown-draft (car spec) (cadr spec))))

(easy-menu-define poly-markdown+R-menu (list ess-mode-map
                                             inferior-ess-mode-map
                                             poly-markdown+R-mode-map)
  "Menu for poly-markdown+R-mode"
  '("RMarkdown"
    ("Templates"
     :active (ess-get-next-available-process "R" t)
     :filter poly-r-rmarkdown-templates-menu)))

(define-key poly-markdown+R-mode-map (kbd "M-n M-m") #'poly-r-rmarkdown-create-from-template)


;; RAPPORT
(defcustom  pm-inner/rapport-YAML
  (pm-inner-chunkmode :name "rapport-YAML"
                      :mode 'yaml-mode
                      :head-matcher "<!--head"
                      :tail-matcher "head-->")
  "YAML header in Rapport files"
  :group 'poly-innermodes
  :type 'object)

;;;###autoload (autoload 'poly-rapport-mode "poly-R")
(define-polymode poly-rapport-mode poly-markdown+R-mode
  :innermodes '(:inherit
                pm-inner/brew-R
                pm-inner/rapport-YAML))


;; HTML
(defcustom  pm-inner/html-R
  (pm-inner-chunkmode :name "html-R"
                      :mode 'R-mode
                      :head-matcher "<!--[ \t]*begin.rcode"
                      :tail-matcher "end.rcode[ \t]*-->")
  "HTML KnitR innermode."
  :group 'poly-innermodes
  :type 'object)

;;;###autoload (autoload 'poly-html+R-mode "poly-R")
(define-polymode poly-html+R-mode pm-poly/html
  :innermodes '(pm-inner/html-R))

;;;###autoload
(defalias 'poly-html+r-mode 'poly-html+R-mode)


;;; R-brew
(defcustom  pm-inner/brew-R
  (pm-inner-chunkmode :name "brew-R"
                      :mode 'R-mode
                      :head-matcher "<%[=%]?"
                      :tail-matcher "[#=%=-]?%>")
  "Brew R chunk."
  :group 'poly-innermodes
  :type 'object)

;;;###autoload (autoload 'poly-brew+R-mode "poly-R")
(define-polymode poly-brew+R-mode pm-poly/brew
  :innermodes '(pm-inner/brew-R))

;;;###autoload
(defalias 'poly-brew+r-mode 'poly-brew+R-mode)


;;; R+C++
;; todo: move into :matcher-subexp functionality?
(defun pm--R+C++-head-matcher (ahead)
  (when (re-search-forward "cppFunction(\\(['\"]\n\\)"
                           nil t ahead)
    (cons (match-beginning 1) (match-end 1))))

(defun pm--R+C++-tail-matcher (ahead)
  (when (< ahead 0)
    (goto-char (car (pm--R+C++-head-matcher -1))))
  (goto-char (max 1 (1- (point))))
  (let ((end (or (ignore-errors (scan-sexps (point) 1))
                 (buffer-end 1))))
    (cons (max 1 (1- end)) end)))

(defcustom  pm-inner/R-C++
  (pm-inner-chunkmode :name "R-C++"
                      :mode 'c++-mode
                      :head-mode 'host
                      :head-matcher 'pm--R+C++-head-matcher
                      :tail-matcher 'pm--R+C++-tail-matcher
                      :protect-font-lock nil)
  "HTML KnitR chunk."
  :group 'poly-innermodes
  :type 'object)

;;;###autoload (autoload 'poly-R+C++-mode "poly-R")
(define-polymode poly-R+C++-mode pm-poly/R
  :innermodes '(pm-inner/R-C++))

;;;###autoload
(defalias 'poly-r+c++-mode 'poly-R+C++-mode)


;;; C++R
(defun pm--C++R-head-matcher (ahead)
  (when (re-search-forward "^[ \t]*/[*]+[ \t]*R" nil t ahead)
    (cons (match-beginning 0) (match-end 0))))

(defun pm--C++R-tail-matcher (ahead)
  (when (< ahead 0)
    (error "backwards tail match not implemented"))
  ;; may be rely on syntactic lookup ?
  (when (re-search-forward "^[ \t]*\\*/")
    (cons (match-beginning 0) (match-end 0))))

(defcustom  pm-inner/C++R
  (pm-inner-chunkmode :name "C++R"
                      :mode 'R-mode
                      :head-matcher 'pm--C++R-head-matcher
                      :tail-matcher 'pm--C++R-tail-matcher)
  "HTML KnitR chunk."
  :group 'polymodes
  :type 'object)

;;;###autoload (autoload 'poly-C++R-mode "poly-R")
(define-polymode poly-C++R-mode pm-poly/C++
  :innermodes '(pm-inner/C++R))

;;;###autoload
(defalias 'poly-c++r-mode 'poly-C++R-mode)


;;; R help
(defcustom  pm-inner/ess-help-R
  (pm-inner-chunkmode :name "ess-help-R"
                      :mode 'R-mode
                      :head-matcher "^Examples:"
                      :tail-matcher "\\'"
                      :indent-offset 5
                      :switch-buffer-functions '(pm--ess-help+R-turn-off-read-only))
  "Ess help R chunk"
  :group 'poly-innermodes
  :type 'object)

(defun pm--ess-help+R-turn-off-read-only (&rest _ignore)
  ;; don't transfer read only status from main help buffer
  (cl-case pm/type
    (body (read-only-mode -1))
    (head (read-only-mode 1))))

;;;###autoload (autoload 'poly-ess-help+R-mode "poly-R")
(define-polymode poly-ess-help+R-mode
  :innermodes '(pm-inner/ess-help-R))

(add-hook 'ess-help-mode-hook (lambda ()
                                (when (string= ess-dialect "R")
                                  (poly-ess-help+R-mode))))


;; Rd examples
(defun pm--Rd-head-matcher (ahead)
  (when (re-search-forward "\\\\\\(examples\\|usage\\) *\\({\n\\)" nil t ahead)
    (cons (match-beginning 0) (match-end 0))))

(defun pm--Rd-tail-matcher (ahead)
  (when (< ahead 0)
    (goto-char (cdr (pm--Rd-head-matcher -1))))
  (let ((end (or (ignore-errors
                   (skip-chars-backward " \t\n{")
                   (scan-sexps (point) 1))
                 (buffer-end 1))))
    (cons (max 1 (- end 1)) end)))

(defcustom pm-inner/Rd-examples
  (pm-inner-chunkmode :name "Rd-examples"
                      :mode 'R-mode
                      :head-mode 'host
                      :head-matcher 'pm--Rd-head-matcher
                      :tail-matcher 'pm--Rd-tail-matcher)
  "Rd examples chunk."
  :group 'poly-innermodes
  :type 'object)

(defcustom pm-host/Rd
  (pm-host-chunkmode :name "Rd"
                     :mode 'Rd-mode)
  "Rd hostmode."
  :group 'poly-hostmodes
  :type 'object)

;;;###autoload (autoload 'poly-Rd-mode "poly-R")
(define-polymode poly-Rd-mode
  :hostmode 'pm-host/Rd
  :innermodes '(pm-inner/Rd-examples))

(add-hook 'Rd-mode-hook 'poly-Rd-mode)


;; Rmarkdown

(defun pm--rmarkdown-output-file-sniffer ()
  (goto-char (point-min))
  (let (files)
    (while (re-search-forward "Output created: +\\(.*\\)" nil t)
      (push (expand-file-name (match-string 1)) files))
    (reverse (delete-dups files))))

(defun pm--rmarkdown-output-file-from-.Last.value ()
  (ess-get-words-from-vector "print(.Last.value)\n"))

(defun pm--rmarkdown-shell-auto-selector (action &rest _ignore)
  (cl-case action
    (doc "AUTO DETECT")
    (command "Rscript -e \"rmarkdown::render('%i', output_format = 'all')\"")
    (output-file #'pm--rmarkdown-output-file-sniffer)))

(defcustom pm-exporter/Rmarkdown
  (pm-shell-exporter :name "Rmarkdown"
                     :from
                     '(("Rmarkdown"  "\\.[rR]?md\\|rapport\\'" "R Markdown"
                        "Rscript -e \"rmarkdown::render('%i', output_format = '%t', output_file = '%o')\""))
                     :to
                     '(("auto" . pm--rmarkdown-shell-auto-selector)
                       ("html" "html" "html document" "html_document")
                       ("pdf" "pdf" "pdf document" "pdf_document")
                       ("word" "docx" "word document" "word_document")
                       ("md" "md" "md document" "md_document")
                       ("ioslides" "html" "ioslides presentation" "ioslides_presentation")
                       ("slidy" "html" "slidy presentation" "slidy_presentation")
                       ("beamer" "pdf" "beamer presentation" "beamer_presentation")))
  "R Markdown exporter.
Please not that with 'AUTO DETECT' export options, output file
names are inferred by Rmarkdown from YAML description
block. Thus, output file names don't comply with
`polymode-exporter-output-file-format'."
  :group 'polymode-export
  :type 'object)

(defun pm--rmarkdown-callback-auto-selector (action &rest _ignore)
  (cl-case action
    (doc "AUTO DETECT")
    ;; last file is not auto-detected unless we cat new line
    (command "rmarkdown::render('%I', output_format = 'all', knit_root_dir=getwd())")
    (output-file #'pm--rmarkdown-output-file-from-.Last.value)))

(defcustom pm-exporter/Rmarkdown-ESS
  (pm-callback-exporter :name "Rmarkdown-ESS"
                        :from
                        '(("Rmarkdown" "\\.[rR]?md\\|rapport\\'" "R Markdown"
                           "rmarkdown::render('%I', output_format = '%t', output_file = '%O', knit_root_dir=getwd())\n"))
                        :to
                        '(("auto" . pm--rmarkdown-callback-auto-selector)
                          ("html" "html" "html document" "html_document")
                          ("pdf" "pdf" "pdf document" "pdf_document")
                          ("word" "docx" "word document" "word_document")
                          ("md" "md" "md document" "md_document")
                          ("ioslides" "html" "ioslides presentation" "ioslides_presentation")
                          ("slidy" "html" "slidy presentation" "slidy_presentation")
                          ("beamer" "pdf" "beamer presentation" "beamer_presentation"))
                        :function 'pm--ess-run-command
                        :callback 'pm--ess-callback)
  "R Markdown exporter.
Please not that with 'AUTO DETECT' export options, output file
names are inferred by Rmarkdown from YAML description
block. Thus, output file names don't comply with
`polymode-exporter-output-file-format'."
  :group 'polymode-export
  :type 'object)

(polymode-register-exporter pm-exporter/Rmarkdown-ESS nil
                            pm-poly/markdown+R)
(polymode-register-exporter pm-exporter/Rmarkdown nil
                            pm-poly/markdown+R)


;;; Bookdown

(defun pm--rbookdown-input-book-selector (action &rest _ignore)
  (cl-case action
    (doc "R Bookdown Book")
    (match (file-exists-p "_bookdown.yml"))
    (command "bookdown::render_book('%I', output_format = '%t')\n")))

(defun pm--rbookdown-intput-chapter-selector (action &rest _ignore)
  (cl-case action
    (doc "R Bookdown Chapter")
    (match (file-exists-p "_bookdown.yml"))
    (command "bookdown::preview_chapter('%I', output_format = '%t')\n")))

(defun pm--rbookdown-output-selector (action id &rest _ignore)
  (cl-case action
    (doc id)
    (output-file #'pm--rmarkdown-output-file-from-.Last.value)
    (t-spec (format "bookdown::%s" id))))

(defcustom pm-exporter/Rbookdown-ESS
  (pm-callback-exporter :name "Rbookdown-ESS"
                        :from
                        '(("Book" . pm--rbookdown-input-book-selector)
                          ("Chapter" . pm--rbookdown-intput-chapter-selector))
                        :to
                        '(("ALL" nil "ALL-CONFIGURED" "all")
                          ("epub_book" . pm--rbookdown-output-selector)
                          ("gitbook" . pm--rbookdown-output-selector)
                          ("html_book" . pm--rbookdown-output-selector)
                          ("html_document2" . pm--rbookdown-output-selector)
                          ("pdf_book" . pm--rbookdown-output-selector)
                          ("pdf_document2" . pm--rbookdown-output-selector)
                          ("tufte_book2" . pm--rbookdown-output-selector)
                          ("tufte_handout2" . pm--rbookdown-output-selector)
                          ("tufte_html2" . pm--rbookdown-output-selector)
                          ("tufte_html_book" . pm--rbookdown-output-selector)
                          ("word_document2" . pm--rbookdown-output-selector))
                        :function 'pm--ess-run-command
                        :callback 'pm--ess-callback)
  "R bookdown exporter within ESS process."
  :group 'polymode-export
  :type 'object)

(polymode-register-exporter pm-exporter/Rbookdown-ESS nil pm-poly/markdown+R)


;; Shiny Rmd

(defun pm--shiny-input-selector (action &optional id &rest _ignore)
  (cl-case action
    (doc "Shiny Rmd Application")
    (match (save-excursion
             (goto-char (point-min))
             (re-search-forward "^[ \t]*runtime:[ \t]+shiny[ \t]*$" nil t)))
    (command (if (equal id "Rmd-ESS")
                 "rmarkdown::run('%I')\n"
               "Rscript -e \"rmarkdown::run('%I')\""))))

(defcustom pm-exporter/Shiny
  (pm-shell-exporter :name "Shiny"
                     :from
                     '(("Rmd" . pm--shiny-input-selector))
                     :to
                     '(("html" "html" "Shiny Web App")))
  "Shiny exporter of Rmd documents in stand alone shell.
The Rmd yaml preamble must contain runtime: shiny declaration."
  :group 'polymode-export
  :type 'object)

(defcustom pm-exporter/Shiny-ESS
  (pm-callback-exporter :name "Shiny-ESS"
                        :from
                        '(("Rmd-ESS" . pm--shiny-input-selector))
                        :to
                        '(("html" "html" "Shiny Web App"))
                        :function 'pm--ess-run-command)
  "Shiny exporter of Rmd documents within ESS process.
The Rmd yaml preamble must contain runtime: shiny declaration."
  :group 'polymode-export
  :type 'object)

(polymode-register-exporter pm-exporter/Shiny nil pm-poly/markdown+R)
(polymode-register-exporter pm-exporter/Shiny-ESS nil pm-poly/markdown+R)


;; KnitR
(defcustom pm-weaver/knitR
  (pm-shell-weaver :name "knitr"
                   :from-to
                   '(("latex" "\\.\\(r?tex\\|rnw\\)\\'" "tex" "LaTeX" "Rscript -e \"knitr::knit('%i', output='%o')\"")
                     ("html" "\\.r?x?html?\\'" "html" "HTML" "Rscript -e \"knitr::knit('%i', output='%o')\"")
                     ("markdown" "\\.r?md]\\'" "md" "Markdown" "Rscript -e \"knitr::knit('%i', output='%o')\"")
                     ("rst" "\\.rst" "rst" "ReStructuredText" "Rscript -e \"knitr::knit('%i', output='%o')\"")
                     ("brew" "\\.r?brew\\'" "brew" "Brew" "Rscript -e \"knitr::knit('%i', output='%o')\"")
                     ("asciidoc" "\\.asciidoc\\'" "txt" "AsciiDoc" "Rscript -e \"knitr::knit('%i', output='%o')\"")
                     ("textile" "\\.textile\\'" "textile" "Textile" "Rscript -e \"knitr::knit('%i', output='%o')\"")))
  "Shell knitR weaver."
  :group 'polymode-weave
  :type 'object)

(polymode-register-weaver pm-weaver/knitR nil
                          pm-poly/noweb+R pm-poly/markdown
                          pm-poly/rapport pm-poly/html+R)

(defcustom pm-weaver/knitR-ESS
  (pm-callback-weaver :name "knitR-ESS"
                      :from-to
                      '(("latex" "\\.\\(r?tex\\|rnw\\)\\'" "tex" "LaTeX" "knitr::knit('%I', output='%O')")
                        ("html" "\\.[xX]?html?\\'" "html" "HTML" "knitr::knit('%I', output='%O')")
                        ("markdown" "\\.r?md]\\'" "md" "Markdown" "knitr::knit('%I', output='%O')")
                        ("rst" "\\.rst\\'" "rst" "ReStructuredText" "knitr::knit('%I', output='%O')")
                        ("brew" "\\.r?brew]\\'" "brew" "Brew" "knitr::knit('%I', output='%O')")
                        ("asciidoc" "\\.r?asciidoc\\'" "txt" "AsciiDoc" "knitr::knit('%I', output='%O')")
                        ("textile" "\\.textile\\'" "textile" "Textile" "knitr::knit('%I', output='%O')"))
                      :function 'pm--ess-run-command
                      :callback 'pm--ess-callback)
  "ESS knitR weaver."
  :group 'polymode-weave
  :type 'object)

(polymode-register-weaver pm-weaver/knitR-ESS nil
                          pm-poly/noweb+R pm-poly/markdown
                          pm-poly/rapport pm-poly/html+R)

(defcustom pm-weaver/Sweave-ESS
  (pm-callback-weaver :name "ESS-Sweave"
                      :from-to '(("latex" "\\.\\(tex\\|r?s?nw\\)\\'" "tex"
                                  "LaTeX" "Sweave('%I', output='%O')"))
                      :function 'pm--ess-run-command
                      :callback 'pm--ess-callback)
  "ESS 'Sweave' weaver."
  :group 'polymode-weave
  :type 'object)

(polymode-register-weaver pm-weaver/Sweave-ESS nil
                          pm-poly/noweb+R)


;; Sweave
(defcustom pm-weaver/Sweave
  (pm-shell-weaver :name "sweave"
                   :from-to
                   '(("latex" "\\.\\(tex\\|r?s?nw\\)\\'"
                      "tex" "LaTeX" "R CMD Sweave %i --options=\"output='%o'\"")))
  "Shell 'Sweave' weaver."
  :group 'polymode-weave
  :type 'object)

(polymode-register-weaver pm-weaver/Sweave nil
                          pm-poly/noweb+R)


;; Setup

(defun poly-r-eval-region (beg end msg)
  (let ((ess-inject-source t))
    (ess-eval-region beg end nil msg)))

(defun poly-r-mode-setup ()
  (when (equal ess-dialect "R")
    (setq-local polymode-eval-region-function #'poly-r-eval-region)))

(add-hook 'ess-mode-hook #'poly-r-mode-setup)


;; ESS command
(declare-function ess-async-command nil)
(declare-function ess-force-buffer-current nil)
(declare-function ess-process-get nil)
(declare-function ess-process-put nil)
(declare-function comint-previous-prompt nil)

(defun pm--ess-callback (proc string)
  (let ((ofile (process-get proc :output-file)))
    ;; This is getting silly. Ess splits output for optimization reasons. So we
    ;; are collecting output from 3 places:
    ;;   - most recent STRING
    ;;   - string in accumulation buffer
    ;;   - string already in output buffer
    (with-current-buffer (if (fboundp 'ess--accumulation-buffer)
                             (ess--accumulation-buffer proc)
                           (process-get proc 'accum-buffer-name))
      (setq string (concat (buffer-string) string)))
    (with-current-buffer (process-buffer proc)
      (setq string (concat (buffer-substring (or ess--tb-last-input (comint-previous-prompt)) (point-max))
                           string)))
    (with-temp-buffer
      (setq ess-dialect "R"
            ess-local-process-name (process-name proc))
      (insert string)
      (when (string-match-p "Error\\(:\\| +in\\)" string)
        (user-error "Errors durring ESS async command"))
      (when (functionp ofile)
        (setq ofile (funcall ofile))))
    ofile))

(defun pm--ess-run-command (command callback &rest _ignore)
  (require 'ess)
  (let ((ess-eval-visibly t)
        (ess-dialect "R"))
    (ess-force-buffer-current)
    (with-current-buffer (ess-get-process-buffer)
      (unless (and (boundp 'goto-address-mode)
                   goto-address-mode)
        ;; mostly for shiny apps (added in ESS 19)
        (goto-address-mode 1)))
    (ess-process-put :output-file pm--output-file)
    (when callback
      (ess-process-put 'callbacks (list callback))
      (ess-process-put 'interruptable? t)
      (ess-process-put 'running-async? t))
    (ess-eval-linewise command)
    (display-buffer (ess-get-process-buffer))))


;; COMPAT
(when (fboundp 'advice-add)
  (advice-add 'ess-eval-paragraph :around 'pm-execute-narrowed-to-span)
  (advice-add 'ess-eval-buffer :around 'pm-execute-narrowed-to-span)
  (advice-add 'ess-beginning-of-function :around 'pm-execute-narrowed-to-span))



;;; ASSOCIATIONS

(add-to-list 'auto-mode-alist '("\\.Snw\\'" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]nw\\'" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]md\\'" . poly-markdown+r-mode))
(add-to-list 'auto-mode-alist '("\\.rapport\\'" . poly-rapport-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]html\\'" . poly-html+r-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]brew\\'" . poly-brew+r-mode))
(add-to-list 'auto-mode-alist '("\\.[Rr]cpp\\'" . poly-r+c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp[rR]\\'" . poly-c++r-mode))

(provide 'poly-R)
