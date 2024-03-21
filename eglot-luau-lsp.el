;;; eglot-luau-lsp.el --- eglot client for luau-lsp -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides a luau-lsp
;; (https://github.com/JohnnyMorganz/luau-lsp) integration for eglot.

;;; Code:

;; Storage directory for type definition and doc resource files
(defconst eglot-luau-lsp-storage-location "~/.emacs.d/luau-lsp-storage/")

;; URL providing Roblox API docs
(defconst eglot-luau-lsp-roblox-api-docs-url "https://raw.githubusercontent.com/MaximumADHD/Roblox-Client-Tracker/roblox/api-docs/en-us.json")

;; Forward declare eglot symbols to silence warnings
(defvar eglot-server-programs)

;; Whether we should update Roblox types.
;; Set to nil after updating docs for a session.
(defvar eglot-luau-lsp-should-update-roblox-types t)

;; Whether we should update Roblox docs.
;; Set to nil after updating docs for a session.
(defvar eglot-luau-lsp-should-update-roblox-docs t)

;; User customization
(defgroup eglot-luau-lsp nil
  "Customization for luau-lsp."
  :prefix 'eglot-luau-lsp-
  :group 'tools)

(defcustom eglot-luau-lsp-sourcemap-includes-non-scripts
  nil
  "Whether to tell Rojo to include non-script instances in its sourcemap."
  :type 'boolean
  :group 'eglot-luau-lsp)

(defcustom eglot-luau-lsp-auto-update-roblox-types
  t
  "Whether to automatically update Roblox types on server initialization."
  :type 'boolean
  :group 'eglot-luau-lsp)

(defcustom eglot-luau-lsp-auto-update-roblox-docs
  t
  "Whether to automatically update Roblox API docs on server initialization."
  :type 'boolean
  :group 'eglot-luau-lsp)

(defcustom eglot-luau-lsp-roblox-security-level
  "PluginSecurity"
  "The security level to use for Roblox API type defintions."
  :type 'string
  :options '("None" "LocalUserSecurity" "PluginSecurity" "RobloxScriptSecurity"))

(defun eglot-luau-lsp-roblox-types-url ()
  "Return a URL that responds with Roblox type information."
  (format
   "http://raw.githubusercontent.com/JohnnyMorganz/luau-lsp/main/scripts/globalTypes.%s.d.luau"
   eglot-luau-lsp-roblox-security-level))

(defun eglot-luau-lsp-roblox-types-storage-uri ()
  "Return where to store type definition files."
  (expand-file-name (concat eglot-luau-lsp-storage-location
                            (format "global-types-%s"
                                    eglot-luau-lsp-roblox-security-level))))

(defun eglot-luau-lsp-roblox-docs-storage-uri ()
  "Return where to store doc files."
  (expand-file-name (concat eglot-luau-lsp-storage-location "api-docs")))

(defun eglot-luau-lsp--url-retrieve-body (url &optional callback)
  "Fetch the content at URL, strip HTTP headers, then run CALLBACK.
The current buffer during CALLBACK contains the body of the response."
  (url-retrieve
   url
   (lambda (_)
     ;; `url-retrieve' writes HTTP headers to the output buffer before
     ;; the response body.  It writes a lone newline at the end of all
     ;; of them, so we'll just search forward until we hit a lone
     ;; newline, and then delete the region from the beginning to the
     ;; lone newline, leaving only the response body in the output
     ;; buffer.  It's probably okay? 😰
     (goto-char (point-min))
     (re-search-forward "^$")
     (delete-region (point) (point-min))
     (if callback (funcall callback)))))

(defun eglot-luau-lsp-update-docs (&optional callback)
  "Fetch and store Roblox API docs, then run CALLBACK."
  (setq eglot-luau-lsp-should-update-roblox-docs nil)
  (eglot-luau-lsp--url-retrieve-body
   eglot-luau-lsp-roblox-api-docs-url
   (lambda ()
     (write-file (eglot-luau-lsp-roblox-docs-storage-uri))
     (kill-buffer)
     (if callback (funcall callback)))))

(defun eglot-luau-lsp-update-roblox-types (&optional callback)
  "Fetch and store Roblox type definitions, then run CALLBACK."
  (setq eglot-luau-lsp-should-update-roblox-types nil)
  (eglot-luau-lsp--url-retrieve-body
   (eglot-luau-lsp-roblox-types-url)
   (lambda ()
     (write-file (eglot-luau-lsp-roblox-types-storage-uri))
     (kill-buffer)
     (if callback (funcall callback)))))

(defun eglot-luau-lsp-add-server-program ()
  "Add luau-lsp as a server program for eglot in lua-mode buffers."
  (add-to-list
   'eglot-server-programs
   `(lua-mode . ("luau-lsp" "lsp"
                 ,(format "--definitions=%s" (eglot-luau-lsp-roblox-types-storage-uri))
                 ,(format "--docs=%s" (eglot-luau-lsp-roblox-docs-storage-uri))))))

;;;###autoload
(defun eglot-luau-lsp-setup (&optional callback)
  "Add luau-lsp as a server program for eglot in lua-mode buffers.

If `eglot-luau-lsp-auto-update-roblox-types' and/or
`eglot-luau-lsp-auto-update-roblox-docs' are enabled, call
CALLBACK once type definitions have been downloaded and saved.
This can be used to start the server (using e.g. `eglot-ensure')
only after type definitions and/or are available."
  (if (not (file-directory-p eglot-luau-lsp-storage-location))
      (make-directory eglot-luau-lsp-storage-location))

  (if (and eglot-luau-lsp-auto-update-roblox-types
           eglot-luau-lsp-should-update-roblox-types)
      (let ((buffer (current-buffer)))
        (eglot-luau-lsp-update-roblox-types
         (lambda ()
           (eglot-luau-lsp-add-server-program)
           (if (and eglot-luau-lsp-auto-update-roblox-docs
                    eglot-luau-lsp-should-update-roblox-docs)
               (eglot-luau-lsp-update-docs
                (lambda ()
                  (if callback (with-current-buffer buffer (funcall callback)))))
             (if callback (with-current-buffer buffer (funcall callback)))))))
    (eglot-luau-lsp-add-server-program)))

(provide 'eglot-luau-lsp)
;;; eglot-luau-lsp.el ends here
