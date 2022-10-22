;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((org-mode . ((eval . (progn
                        (setq org-download-heading-lvl nil)
                        (setq org-download-image-dir
                              (concat (projectile-project-root) "images")))))))
