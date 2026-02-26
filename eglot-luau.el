;;; eglot-luau.el --- Luau language server integration for eglot -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kenneth Loeffler

;; Author: Kenneth Loeffler <kenloef@gmail.com>
;; Version: 0.2.0
;; Keywords: roblox, luau, tools
;; URL: https://github.com/kennethloeffler/eglot-luau
;; Package-Requires: ((emacs "29.1") (eglot "1.17"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides a luau-lsp
;; (https://github.com/JohnnyMorganz/luau-lsp) integration for eglot.

;;; Code:
(require 'eglot)
(require 'json)

;; Storage directory for type definition and doc resource files
(defconst eglot-luau-storage-directory (concat user-emacs-directory
                                               "luau-lsp-storage/"))

;; URL providing Roblox API docs
(defconst eglot-luau-roblox-docs-url "https://raw.githubusercontent.com/MaximumADHD/Roblox-Client-Tracker/roblox/api-docs/en-us.json")

;; URL providing latest Roblox version
(defconst eglot-luau-roblox-version-url "https://raw.githubusercontent.com/CloneTrooper1019/Roblox-Client-Tracker/roblox/version.txt")

;; URL providing Roblox's current Luau FFlag configuration
(defconst eglot-luau-current-roblox-fflags-url
  "https://clientsettingscdn.roblox.com/v1/settings/application?applicationName=PCDesktopClient")

;; User customization
(defgroup eglot-luau nil
  "Customization for luau-lsp."
  :prefix 'eglot-luau-
  :group 'tools)

(defcustom eglot-luau-rojo-sourcemap-enabled
  nil
  "Whether to use Rojo to generate a sourcemap for projects."
  :type 'boolean
  :group 'eglot-luau)

(defcustom eglot-luau-rojo-sourcemap-includes-non-scripts
  nil
  "Whether to tell Rojo to include non-script instances in its sourcemap."
  :type 'boolean
  :group 'eglot-luau)

(defcustom eglot-luau-auto-update-roblox-types
  nil
  "Whether to automatically update Roblox types on server initialization."
  :type 'boolean
  :group 'eglot-luau)

(defcustom eglot-luau-auto-update-roblox-docs
  nil
  "Whether to automatically update Roblox API docs on server initialization."
  :type 'boolean
  :group 'eglot-luau)

(defcustom eglot-luau-roblox-security-level
  "PluginSecurity"
  "The security level to use for Roblox API type defintions."
  :type 'string
  :options '("None" "LocalUserSecurity" "PluginSecurity" "RobloxScriptSecurity")
  :group 'eglot-luau)

(defcustom eglot-luau-rojo-project-path
  "default.project.json"
  "A relative path to the Rojo project to use for sourcemap generation."
  :type 'string
  :group 'eglot-luau)

(defcustom eglot-luau-fflags-enabled
  t
  "Whether the language server will have any FFlags enabled."
  :type 'boolean
  :group 'eglot-luau)

(defcustom eglot-luau-fflag-overrides
  '()
  "Custom set FFlags passed to the language server."
  :type '(alist :key-type (string :tag "Name")
                :value-type (string :tag "Value"))
  :group 'eglot-luau)

(defcustom eglot-luau-custom-type-files
  '()
  "List of paths to type definition files to provide to the language server."
  :type '(repeat string)
  :group 'eglot-luau)

(defcustom eglot-luau-custom-doc-files
  '()
  "List of paths to doc files to provide to the language server."
  :type '(repeat string)
  :group 'eglot-luau)

(defcustom eglot-luau-server-executable
  "luau-lsp"
  "Path to the luau-lsp server executable."
  :type 'string
  :group 'eglot-luau)

(defcustom eglot-luau-sync-fflags
  t
  "Whether to sync Luau FFlags with Roblox's current configuration."
  :type 'boolean
  :group 'eglot-lua)

(defun eglot-luau--ensure-storage-directory ()
  "Create luau-lsp storage folder if it doesn't exist."
  (when (not (file-directory-p eglot-luau-storage-directory))
    (make-directory eglot-luau-storage-directory)))

(defun eglot-luau--roblox-types-url ()
  "Return a URL that responds with Roblox type information."
  (format
   "http://raw.githubusercontent.com/JohnnyMorganz/luau-lsp/main/scripts/globalTypes.%s.d.luau"
   eglot-luau-roblox-security-level))

(defun eglot-luau--roblox-types-storage-uri ()
  "Return where to store type definition files."
  (expand-file-name (concat eglot-luau-storage-directory
                            (format "roblox-global-types-%s"
                                    eglot-luau-roblox-security-level))))

(defun eglot-luau--roblox-docs-storage-uri ()
  "Return where to store doc files."
  (expand-file-name (concat eglot-luau-storage-directory "roblox-api-docs")))


(defun eglot-luau--roblox-version-storage-uri ()
  "Return where to store version files."
  (expand-file-name (concat eglot-luau-storage-directory "roblox-version")))

(defun eglot-luau--which-files-need-update ()
  "Return a list of bools that indicate which Roblox resources should be updated.
Each bool in the list indicates whether the types or docs file
need updates, respectively."
  (list (and eglot-luau-auto-update-roblox-types
             (not (file-exists-p (eglot-luau--roblox-types-storage-uri))))
        (and eglot-luau-auto-update-roblox-docs
             (not (file-exists-p (eglot-luau--roblox-docs-storage-uri))))))

(defun eglot-luau--is-outdated ()
  "Compare versions of locally stored Roblox docs/types with the latest versions.
Return a list of bools that indicate whether the types and/or
docs files, respectively, need to be updated.  Respects the
`eglot-luau-auto-update-roblox-types' and
`eglot-luau-auto-update-roblox-docs' settings."
  (if (not (or eglot-luau-auto-update-roblox-types
               eglot-luau-auto-update-roblox-docs))
      ;; Don't do anything if user doesn't want Roblox types or docs
      '(nil nil)
    (eglot-luau--ensure-storage-directory)
    (with-temp-buffer
      (url-insert-file-contents eglot-luau-roblox-version-url)
      (let* ((version-file (eglot-luau--roblox-version-storage-uri))
             (stored-version (if (file-exists-p version-file)
                                 (with-temp-buffer (insert-file-contents version-file)
                                                   (buffer-string))
                               "")))
        (write-file version-file)
        (if (not (string= stored-version (buffer-string)))
            (list eglot-luau-auto-update-roblox-types
                  eglot-luau-auto-update-roblox-docs)
          (eglot-luau--which-files-need-update))))))

(defun eglot-luau--build-server-command-list ()
  "Return a list of strings that used to spawn the luau-lsp server process."
  (let ((command-list (list "lsp" eglot-luau-server-executable)))
    (let ((types-uri (eglot-luau--roblox-types-storage-uri))
          (docs-uri (eglot-luau--roblox-docs-storage-uri)))
      (when (file-exists-p types-uri)
        (push (format "--definitions=%s" types-uri) command-list))
      (when (file-exists-p docs-uri)
        (push (format "--docs=%s" docs-uri) command-list)))
    (when eglot-luau-custom-type-files
      (dolist (file eglot-luau-custom-type-files)
        (push (format "--definitions=%s" file) command-list)))
    (when eglot-luau-custom-doc-files
      (dolist (file eglot-luau-custom-doc-files)
        (push (format "--docs=%s" file) command-list)))
    (unless eglot-luau-fflags-enabled
      (push "--no-flags-enabled" command-list))
    (when eglot-luau-fflag-overrides
      (dolist (fflag eglot-luau-fflag-overrides)
        (push (format "--flag:%s=%s" (car fflag) (cadr fflag)) command-list)))
    (when eglot-luau-sync-fflags
      (let ((fflags (cdar (with-temp-buffer
                            (with-demoted-errors
                                "Error while fetching Roblox FFlags: %s"
                              (url-insert-file-contents
                               eglot-luau-current-roblox-fflags-url)
                              (json-read))))))
        (dolist (fflag fflags)
          (let* ((name (symbol-name (car fflag)))
                 (trimmed-name (if (string-prefix-p "FFlagLuau" name)
                                   (string-trim-left name "FFlag")
                                 name))
                 (value (cdr fflag)))
            (unless (assoc trimmed-name eglot-luau-fflag-overrides)
              (push (format "--flag:%s=%s" trimmed-name value)
                    command-list))))))
    (nreverse command-list)))

(defun eglot-luau--build-rojo-command-list ()
  "Return a list of strings that can be used to start the Rojo process."
  (let ((command-list (list "--watch"
                            "sourcemap.json" "--output"
                            "sourcemap" "rojo")))
    (push (expand-file-name eglot-luau-rojo-project-path)
          command-list)
    (when eglot-luau-rojo-sourcemap-includes-non-scripts
      (push "--include-non-scripts" command-list))
    (nreverse command-list)))

(defun eglot-luau--rojo-process-filter (_process output)
  "Process filter that displays any errors during Rojo sourcemap generation.
If OUTPUT contains an error message, display the output in a pop-up buffer."
  (when (string-match "error" output)
    (with-output-to-temp-buffer (get-buffer-create
                                 "*luau-lsp sourcemap error*")
      (princ
       (format
        "eglot-luau attempted to generate a sourcemap using the following command:

%s

...but the command outputted an error:

%s"
        (mapconcat #'identity (eglot-luau--build-rojo-command-list) " ")
        output)))))

(defun eglot-luau--make-rojo-process (mode)
  "Return a handler for Rojo for servers running in MODE buffers.

Server must support Luau in MODE buffers.  Fails when Rojo is not
installed, or when a file at `eglot-luau-rojo-project-path' cannot be
found."
  (lambda (server &rest _)
    (if-let* ((is-sourcemap-enabled eglot-luau-rojo-sourcemap-enabled)
              (is-luau-server (member `(,(identity mode) . "Luau") (slot-value server 'languages)))
              (is-rojo-installed (executable-find "rojo")))
        (let* ((rojo-process (make-process
                              :name "luau-lsp-rojo-sourcemap"
                              :command (eglot-luau--build-rojo-command-list)
                              :filter #'eglot-luau--rojo-process-filter
                              :noquery t))
               (advice-name (concat (process-name rojo-process)
                                    "-shutdown-advice")))
          ;; eglot does not provide any hooks for server shutdown, so to
          ;; know when to kill the Rojo process, we have to advise
          ;; `eglot-shutdown' temporarily. This is a bit ugly and goes
          ;; against elisp code conventions, but it's the only way to
          ;; do it right now!
          (advice-add
           #'eglot-shutdown
           :after (lambda (server-shutting-down &rest _)
                    (when (eq server server-shutting-down)
                      (if (process-live-p rojo-process)
                          (kill-process rojo-process))
                      (advice-remove #'eglot-shutdown advice-name)))
           '((name . advice-name))))
      (when (and is-sourcemap-enabled
                 is-luau-server
                 (not is-rojo-installed))
        (with-output-to-temp-buffer (get-buffer-create
                                     "*luau-lsp sourcemap error*")
          (princ "eglot-luau-rojo-sourcemap-enabled is non-nil, but Rojo is not on the path"))))))

(defun eglot-luau--update-resource (kind)
  "Attempt to fetch the resource KIND and store the result on disk."
  (let* ((name (symbol-name kind))
         (fetch-url (intern (format "eglot-luau--roblox-%s-url" name)))
         (storage-uri (intern (format "eglot-luau--roblox-%s-storage-uri" name)))
         (error-message (concat (format "Error while updating Roblox global %s: " name)
                                "%s")))
    (with-temp-buffer
      (with-demoted-errors error-message
        (url-insert-file-contents (funcall (symbol-function fetch-url)))
        (write-file (funcall (symbol-function storage-uri)))))))

;;;###autoload
(defun eglot-luau-setup (mode)
  "Set up luau-lsp for use in MODE buffers.
If `eglot-luau-auto-update-roblox-types' and/or
`eglot-luau-auto-update-roblox-docs' are non-nil, attempt to
download latest Roblox type defintions and/or docs.

If `eglot-luau-rojo-sourcemap-enabled' is non-nil, attempt to
start a Rojo process to generate a sourcemap."
  (pcase-let ((`(,types-need-update ,docs-need-update)
               (with-demoted-errors
                   "Error while fetching Roblox version: %s"
                 (eglot-luau--is-outdated))))
    (when types-need-update (eglot-luau--update-resource 'types))
    (when docs-need-update (eglot-luau--update-resource 'docs))
    (add-hook 'eglot-server-initialized-hook (eglot-luau--make-rojo-process mode))
    (add-to-list 'eglot-server-programs
                 `((,(identity mode) :language-id "Luau")
                   . ,(eglot-luau--build-server-command-list)))))

(provide 'eglot-luau)
;;; eglot-luau.el ends here
