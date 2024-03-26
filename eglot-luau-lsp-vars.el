;;; eglot-luau-lsp-vars.el --- variables for luau-lsp eglot client -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kenneth Loeffler

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
;; This file contains variables and configuration for eglot-luau-lsp.

;;; Code:

;; Storage directory for type definition and doc resource files
(defconst eglot-luau-lsp-storage-location "~/.emacs.d/luau-lsp-storage/")

;; URL providing Roblox API docs
(defconst eglot-luau-lsp-roblox-docs-url "https://raw.githubusercontent.com/MaximumADHD/Roblox-Client-Tracker/roblox/api-docs/en-us.json")

;; URL providing latest Roblox version
(defconst eglot-luau-lsp-roblox-version-url "https://raw.githubusercontent.com/CloneTrooper1019/Roblox-Client-Tracker/roblox/version.txt")

;; Forward declare eglot variables to silence warnings
(defvar eglot-server-programs)

;; User customization
(defgroup eglot-luau-lsp nil
  "Customization for luau-lsp."
  :prefix 'eglot-luau-lsp-
  :group 'tools)

(defcustom eglot-luau-lsp-rojo-sourcemap-enabled
  nil
  "Whether to use Rojo to generate a sourcemap for projects."
  :type 'boolean
  :group 'eglot-luau-lsp)

(defcustom eglot-luau-lsp-rojo-sourcemap-includes-non-scripts
  nil
  "Whether to tell Rojo to include non-script instances in its sourcemap."
  :type 'boolean
  :group 'eglot-luau-lsp)

(defcustom eglot-luau-lsp-auto-update-roblox-types
  nil
  "Whether to automatically update Roblox types on server initialization."
  :type 'boolean
  :group 'eglot-luau-lsp)

(defcustom eglot-luau-lsp-auto-update-roblox-docs
  nil
  "Whether to automatically update Roblox API docs on server initialization."
  :type 'boolean
  :group 'eglot-luau-lsp)

(defcustom eglot-luau-lsp-roblox-security-level
  "PluginSecurity"
  "The security level to use for Roblox API type defintions."
  :type 'string
  :options '("None" "LocalUserSecurity" "PluginSecurity" "RobloxScriptSecurity")
  :group 'eglot-luau-lsp)

(defcustom eglot-luau-lsp-rojo-project-path
  "default.project.json"
  "A relative path to the Rojo project to use for sourcemap generation."
  :type 'string
  :group 'eglot-luau-lsp)

(defcustom eglot-luau-lsp-flags-enabled
  t
  "Whether the language server will have FFlags enabled."
  :type 'boolean
  :group 'eglot-luau-lsp)

(defcustom eglot-luau-lsp-custom-set-flags
  '()
  "Custom set FFlags passed to the language server."
  :type '(alist :key-type (string :tag "Name")
                :value-type (string :tag "Value"))
  :group 'eglot-luau-lsp)

(provide 'eglot-luau-lsp-vars)
;;; eglot-luau-lsp-vars.el ends here
