
(require 'poly-R)
(require 'polymode-test-utils)

(ert-deftest poly-R/indentation ()
  (pm-test-file-indent poly-noweb+R-mode "knitr-beamer.Rnw"))

(ert-deftest poly-R/Rd-mode-test ()
  (pm-test-run-on-file poly-Rd-mode "test.Rd"
    (switch-to-buffer (current-buffer))
    (goto-char 1)
    (pm-switch-to-buffer)
    (should (eq major-mode 'Rd-mode))
    (goto-char 52)
    (pm-switch-to-buffer)
    (should (eq major-mode 'ess-r-mode))
    (goto-char 92)
    (pm-switch-to-buffer)
    (should (eq major-mode 'Rd-mode))
    (goto-char 102)
    (pm-switch-to-buffer)
    (should (eq major-mode 'ess-r-mode))))

(ert-deftest poly-R/Rd-font-lock-test ()
  (pm-test-poly-lock poly-Rd-mode "test.Rd"
    ((insert-delete-1 ("usage" end))
     (switch-to-buffer (current-buffer))
     (insert " ")
     (pm-test-spans)
     (delete-backward-char 1))
    ((insert-delete-2 ("^a_name" beg))
     (insert "  ")
     (delete-backward-char 1))
    ((insert-new-line-3 ("examples" end))
     (insert "\n")
     (pm-test-spans)
     (delete-backward-char 1))))

(ert-deftest poly-R/no-nested-spans ()
  (pm-test-run-on-string 'poly-markdown-mode
    "```{r load-bodipy}
cnames = tolower(cnames)
cnames = gsub('\\(', '', cnames)
cnames = gsub('aa', 'bb', cnames)
```"
    (switch-to-buffer (current-buffer))
    (goto-char 20)
    (pm-switch-to-buffer)
    (should (eq major-mode 'ess-r-mode))
    (should (equal (pm-innermost-range 20 t)
                   (cons 20 111)))
    (goto-char 78)
    (pm-switch-to-buffer)
    (should (eq major-mode 'ess-r-mode))))

(ert-deftest poly-R/after-change-extention ()
  (pm-test-poly-lock poly-markdown+R-mode "minimal.Rmd"
    ((delete-1 ("first\n```" end))
     (delete-backward-char 1)
     (pm-test-spans)
     (insert "`")
     (pm-test-spans))))

