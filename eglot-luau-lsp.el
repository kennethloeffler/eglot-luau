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

(defun eglot-luau-lsp--which-files-need-update ()
  "Return a list of bools that indicate whether type or doc files (respectively) should be updated."
  (list (and eglot-luau-lsp-auto-update-roblox-types
             (not (file-exists-p (eglot-luau-lsp-roblox-types-storage-uri))))
        (and eglot-luau-lsp-auto-update-roblox-docs
             (not (file-exists-p (eglot-luau-lsp-roblox-docs-storage-uri))))))

(defun eglot-luau-lsp-is-outdated ()
  "Compare versions of locally stored Roblox docs/types with the latest versions.
Return a list of bools that indicate whether the types and/or
docs files, respectively, need to be updated.  Respects the
`eglot-luau-lsp-auto-update-roblox-types' and
`eglot-luau-lsp-auto-update-roblox-docs' settings."
  (if (not (or eglot-lua-lsp-auto-update-roblox-types
               eglot-luau-lsp-auto-update-roblox-docs))
      ;; Don't do anything if user doesn't want Roblox types or docs
      '(nil nil)
    (progn
      (eglot-luau-lsp--ensure-storage)
      (with-temp-buffer
        (url-insert-file-contents eglot-luau-lsp-roblox-version-url)
        (let ((version-file (eglot-luau-lsp-roblox-version-storage-uri)))
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

(defun eglot-luau-lsp-update-roblox-docs ()
  "Download and store latest Roblox API docs."
  (with-temp-buffer
    (url-insert-file-contents eglot-luau-lsp-roblox-docs-url)
    (write-file (eglot-luau-lsp-roblox-docs-storage-uri))))

(defun eglot-luau-lsp-update-roblox-types ()
  "Download and store latest Roblox type definitions."
  (with-temp-buffer
    (url-insert-file-contents (eglot-luau-lsp-roblox-types-url))
    (write-file (eglot-luau-lsp-roblox-types-storage-uri))))

(defun eglot-luau-lsp--build-server-command-list ()
  "Return a list of strings that can be used to spawn the luau-lsp server process."
  (let ((command-list (list "lsp" "luau-lsp"))
        (types-file (eglot-luau-lsp-roblox-types-storage-uri))
        (docs-file (eglot-luau-lsp-roblox-docs-storage-uri)))
    (progn
      (if (file-exists-p types-file)
          (push (format "--definitions=%s" types-file) command-list))
      (if (file-exists-p docs-file)
          (push (format "--docs=%s" docs-file) command-list))
      (nreverse command-list))))

(defun eglot-luau-lsp-add-server-program ()
  "Add luau-lsp as an eglot server program for lua-mode buffers."
  (add-to-list 'eglot-server-programs
               `(lua-mode . ,(eglot-luau-lsp--build-server-command-list))))

;;;###autoload
(defun eglot-luau-lsp-setup ()
  "Add luau-lsp as a server program for eglot in lua-mode buffers."
  (pcase-let ((`(,types-need-update ,docs-need-update)
               (ignore-errors (eglot-luau-lsp-is-outdated))))
    (progn
      (if types-need-update
          (ignore-errors (eglot-luau-lsp-update-roblox-types)))
      (if docs-need-update
          (ignore-errors (eglot-luau-lsp-update-roblox-docs)))))
  (eglot-luau-lsp-add-server-program))

(provide 'eglot-luau-lsp)
;;; eglot-luau-lsp.el ends here
