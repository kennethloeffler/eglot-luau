;;; eglot-luau-lsp.el --- eglot client for luau-lsp -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides a luau-lsp
;; (https://github.com/JohnnyMorganz/luau-lsp) integration for eglot.

;;; Code:
(require 'eglot-luau-lsp-vars)

(defun eglot-luau-lsp--ensure-storage ()
  "Create luau-lsp storage folder if it doesn't exist."
  (if (not (file-directory-p eglot-luau-lsp-storage-location))
      (make-directory eglot-luau-lsp-storage-location)))

(defun eglot-luau-lsp-roblox-types-url ()
  "Return a URL that responds with Roblox type information."
  (format
   "http://raw.githubusercontent.com/JohnnyMorganz/luau-lsp/main/scripts/globalTypes.%s.d.luau"
   eglot-luau-lsp-roblox-security-level))

(defun eglot-luau-lsp-roblox-types-storage-uri ()
  "Return where to store type definition files."
  (expand-file-name (concat eglot-luau-lsp-storage-location
                            (format "roblox-global-types-%s"
                                    eglot-luau-lsp-roblox-security-level))))

(defun eglot-luau-lsp-roblox-docs-storage-uri ()
  "Return where to store doc files."
  (expand-file-name (concat eglot-luau-lsp-storage-location "roblox-api-docs")))


(defun eglot-luau-lsp-roblox-version-storage-uri ()
  "Return where to store version files."
  (expand-file-name (concat eglot-luau-lsp-storage-location "roblox-version")))

(defun eglot-luau-lsp-is-outdated ()
  "Fetch and compare locally stored Roblox version with the latest."
  (eglot-luau-lsp--ensure-storage)
  (if eglot-luau-lsp-has-checked-version
      nil
    (progn
      (setq eglot-luau-lsp-has-checked-version t)
      (with-temp-buffer
        (url-insert-file-contents eglot-luau-lsp-roblox-version-url)
        (let ((version-file (eglot-luau-lsp-roblox-version-storage-uri)))
          (if (file-exists-p version-file)
              (let ((stored-version (with-temp-buffer
                                      (insert-file-contents version-file)
                                      (buffer-string))))
                (or (not (file-exists-p (eglot-luau-lsp-roblox-types-storage-uri)))
                    (not (file-exists-p (eglot-luau-lsp-roblox-docs-storage-uri)))
                    (not (string= stored-version (buffer-string)))))
            (progn
              (write-file version-file)
              t)))))))

(defun eglot-luau-lsp-update-roblox-docs ()
  "Fetch and store Roblox API docs."
  (with-temp-buffer
    (url-insert-file-contents eglot-luau-lsp-roblox-docs-url)
    (write-file (eglot-luau-lsp-roblox-docs-storage-uri))))

(defun eglot-luau-lsp-update-roblox-types ()
  "Fetch and store Roblox type definitions."
  (with-temp-buffer
    (url-insert-file-contents (eglot-luau-lsp-roblox-types-url))
    (write-file (eglot-luau-lsp-roblox-types-storage-uri))))

(defun eglot-luau-lsp-add-server-program ()
  "Add luau-lsp as an eglot server program lua-mode buffers."
  (add-to-list
   'eglot-server-programs
   `(lua-mode . ("luau-lsp" "lsp"
                 ,(format "--definitions=%s" (eglot-luau-lsp-roblox-types-storage-uri))
                 ,(format "--docs=%s" (eglot-luau-lsp-roblox-docs-storage-uri))))))

;;;###autoload
(defun eglot-luau-lsp-setup ()
  "Add luau-lsp as a server program for eglot in lua-mode buffers."
  (if (ignore-errors (eglot-luau-lsp-is-outdated))
      (progn (ignore-errors (eglot-luau-lsp-update-roblox-types))
             (ignore-errors (eglot-luau-lsp-update-roblox-docs))))
  (eglot-luau-lsp-add-server-program))

(provide 'eglot-luau-lsp)
;;; eglot-luau-lsp.el ends here
