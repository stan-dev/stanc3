;; Support for compiling in subdirectories from Emacs. Adapted from Coq source.
((nil
  . ((eval
      . (progn
          ;; root directory (ending with slash)
          (let ((stan-root-directory
                 (when buffer-file-name
                   (locate-dominating-file buffer-file-name ".dir-locals.el")))
                (stan-project-find-file
                 (and (boundp 'stan-project-find-file) stan-project-find-file)))

            ;; stan tags file
            (when stan-root-directory
              (setq tags-file-name (concat stan-root-directory "TAGS"))
              (add-to-list 'compilation-search-path stan-root-directory)
              ;; Setting the compilation directory to stan root. This is
              ;; mutually exclusive with the setting of default-directory
              ;; below.
              (if (not stan-project-find-file)
                  (setq compile-command (concat "make -C " stan-root-directory)))
              )
            (setq stan-executable (concat stan-root-directory "all")))))))
)
