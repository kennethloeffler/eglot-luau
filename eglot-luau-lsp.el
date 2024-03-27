;;; eglot-luau-lsp.el --- eglot client for luau-lsp -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kenneth Loeffler

;; Author: Kenneth Loeffler <kenloef@gmail.com>
;; Version: 0.1.0
;; Keywords: roblox, luau, tools
;; URL: https://github.com/kennethloeffler/eglot-luau-lsp
;; Package-Requires: ((emacs "29.1"))

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
(require 'eglot-luau-lsp-vars)

(defun eglot-luau-lsp--ensure-storage ()
  "Create luau-lsp storage folder if it doesn't exist."
  (if (not (file-directory-p eglot-luau-lsp-storage-location))
      (make-directory eglot-luau-lsp-storage-location)))

(defun eglot-luau-lsp--roblox-types-url ()
  "Return a URL that responds with Roblox type information."
  (format
   "http://raw.githubusercontent.com/JohnnyMorganz/luau-lsp/main/scripts/globalTypes.%s.d.luau"
   eglot-luau-lsp-roblox-security-level))

(defun eglot-luau-lsp--roblox-types-storage-uri ()
  "Return where to store type definition files."
  (expand-file-name (concat eglot-luau-lsp-storage-location
                            (format "roblox-global-types-%s"
                                    eglot-luau-lsp-roblox-security-level))))

(defun eglot-luau-lsp--roblox-docs-storage-uri ()
  "Return where to store doc files."
  (expand-file-name (concat eglot-luau-lsp-storage-location "roblox-api-docs")))


(defun eglot-luau-lsp--roblox-version-storage-uri ()
  "Return where to store version files."
  (expand-file-name (concat eglot-luau-lsp-storage-location "roblox-version")))

(defun eglot-luau-lsp--which-files-need-update ()
  "Return a list of bools that indicate which Roblox resources should be updated.
Each bool in the list indicates whether the types or docs file
need updates, respectively."
  (list (and eglot-luau-lsp-auto-update-roblox-types
             (not (file-exists-p (eglot-luau-lsp--roblox-types-storage-uri))))
        (and eglot-luau-lsp-auto-update-roblox-docs
             (not (file-exists-p (eglot-luau-lsp--roblox-docs-storage-uri))))))

(defun eglot-luau-lsp--is-outdated ()
  "Compare versions of locally stored Roblox docs/types with the latest versions.
Return a list of bools that indicate whether the types and/or
docs files, respectively, need to be updated.  Respects the
`eglot-luau-lsp-auto-update-roblox-types' and
`eglot-luau-lsp-auto-update-roblox-docs' settings."
  (if (not (or eglot-luau-lsp-auto-update-roblox-types
               eglot-luau-lsp-auto-update-roblox-docs))
      ;; Don't do anything if user doesn't want Roblox types or docs
      '(nil nil)
    (progn
      (eglot-luau-lsp--ensure-storage)
      (with-temp-buffer
        (url-insert-file-contents eglot-luau-lsp-roblox-version-url)
        (let ((version-file (eglot-luau-lsp--roblox-version-storage-uri)))
          (if (file-exists-p version-file)
              (let ((stored-version (with-temp-buffer
                                      (insert-file-contents version-file)
                                      (buffer-string))))
                (if (not (string= stored-version (buffer-string)))
                    (list eglot-luau-lsp-auto-update-roblox-types
                          eglot-luau-lsp-auto-update-roblox-docs)
                  (eglot-luau-lsp--which-files-need-update)))
            (progn
              (write-file version-file)
              (eglot-luau-lsp--which-files-need-update))))))))

(defun eglot-luau-lsp--build-server-command-list ()
  "Return a list of strings that used to spawn the luau-lsp server process."
  (let ((command-list (list "lsp" eglot-luau-lsp-server-executable))
        (types-file (eglot-luau-lsp--roblox-types-storage-uri))
        (docs-file (eglot-luau-lsp--roblox-docs-storage-uri)))
    (if (file-exists-p types-file)
        (push (format "--definitions=%s" types-file) command-list))
    (if (file-exists-p docs-file)
        (push (format "--docs=%s" docs-file) command-list))
    (if eglot-luau-lsp-custom-type-files
        (dolist (file eglot-luau-lsp-custom-type-files)
          (push (format "--definitions=%s" file) command-list)))
    (if eglot-luau-lsp-custom-doc-files
        (dolist (file eglot-luau-lsp-custom-doc-files)
          (push (format "--docs=%s" file) command-list)))
    (if (not eglot-luau-lsp-flags-enabled)
        (push "--no-flags-enabled" command-list))
    (if eglot-luau-lsp-custom-set-flags
        (dolist (flag eglot-luau-lsp-custom-set-flags)
          (push (format "--flag:%s=%s" (car flag) (cadr flag)) command-list)))
    (nreverse command-list)))

(defun eglot-luau-lsp--build-rojo-command-list ()
  "Return a list of strings that can be used to start the Rojo process."
  (let ((command-list (list "--watch"
                            "sourcemap.json" "--output"
                            "sourcemap" "rojo")))
    (push (expand-file-name eglot-luau-lsp-rojo-project-path)
          command-list)
    (if eglot-luau-lsp-rojo-sourcemap-includes-non-scripts
        (push "--include-non-scripts" command-list))
    (nreverse command-list)))

(defun eglot-luau-lsp--rojo-process-filter (_process output)
  "Process filter that displays any errors during Rojo sourcemap generation.
If OUTPUT contains an error message, display the output in a pop-up buffer."
  (if (string-match "error" output)
      (with-output-to-temp-buffer (get-buffer-create
                                   "*luau-lsp sourcemap error*")
        (princ
         (format
          "eglot-luau-lsp attempted to generate a sourcemap using the following command:

%s

...but the command outputted an error:

%s"
          (mapconcat 'identity (eglot-luau-lsp--build-rojo-command-list) " ")
          output)))))

(defun eglot-luau-lsp--make-rojo-process (server &rest _)
  "Handle the Rojo process for SERVER.
SERVER must have a language-id equal to \"luau\". Fails when Rojo
is not installed, or when a file at
`eglot-luau-lsp-rojo-project-path' cannot be found."
  (if-let ((is-sourcemap-enabled eglot-luau-lsp-rojo-sourcemap-enabled)
           (is-luau-server (string= (slot-value server 'language-id) "luau"))
           (is-rojo-installed (executable-find "rojo")))
      (let ((rojo-process (make-process
                           :name "luau-lsp-rojo-sourcemap"
                           :command (eglot-luau-lsp--build-rojo-command-list)
                           :filter #'eglot-luau-lsp--rojo-process-filter
                           :noquery t)))
        ;; eglot does not provide any hooks for server shutdown, so to
        ;; know when to kill the Rojo process, we have to advise
        ;; `eglot-shutdown' temporarily. This is a bit ugly and goes
        ;; against elisp code conventions, but it's the only way to
        ;; do it right now!
        (advice-add
         'eglot-shutdown
         :after (lambda (server-shutting-down &rest _)
                  (if (eq server server-shutting-down)
                      (progn (if (process-live-p rojo-process)
                                 (kill-process rojo-process))
                             (advice-remove 'eglot-shutdown "kill-rojo"))))
         '((name . "kill-rojo"))))
    (if (and is-sourcemap-enabled
             (not is-rojo-installed))
        (with-output-to-temp-buffer (get-buffer-create
                                     "*luau-lsp sourcemap error*")
          (princ "eglot-luau-lsp-rojo-sourcemap-enabled is non-nil, but Rojo is not on the path")))))

;;;###autoload
(defun eglot-luau-lsp-add-server-program ()
  "Add luau-lsp as an eglot server program for `lua-mode' buffers."
  (add-to-list 'eglot-server-programs
               `((lua-mode :language-id "luau")
                 . ,(eglot-luau-lsp--build-server-command-list))))

;;;###autoload
(defun eglot-luau-lsp-setup ()
  "Add luau-lsp as a server program for eglot in `lua-mode' buffers."
  (pcase-let ((`(,types-need-update ,docs-need-update)
               (with-demoted-errors
                   "Error while fetching Roblox version: %s"
                 (eglot-luau-lsp--is-outdated))))
    (if types-need-update
        (with-temp-buffer
          (with-demoted-errors
              "Error while updating Roblox global types: %s"
            (url-insert-file-contents (eglot-luau-lsp--roblox-types-url))
            (write-file (eglot-luau-lsp--roblox-types-storage-uri)))))
    (if docs-need-update
        (with-temp-buffer
          (with-demoted-errors
              "Error while updating Roblox docs: %s"
            (url-insert-file-contents eglot-luau-lsp-roblox-docs-url)
            (write-file (eglot-luau-lsp--roblox-docs-storage-uri))))))
  (add-hook 'eglot-server-initialized-hook #'eglot-luau-lsp--make-rojo-process)
  (eglot-luau-lsp-add-server-program))

(provide 'eglot-luau-lsp)
;;; eglot-luau-lsp.el ends here
