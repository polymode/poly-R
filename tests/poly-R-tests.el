
(require 'poly-R)
(require 'polymode-test-utils)

(ert-deftest poly-R/indentation ()
  (pm-test-file-indent poly-noweb+R-mode "knitr-beamer-orig.Rnw" "knitr-beamer-indented.Rnw"))
