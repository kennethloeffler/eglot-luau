;;; eglot-luau-lsp-vars.el --- variables for luau-lsp eglot client -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains variables and configuration for eglot-luau-lsp.

;;; Code:

;; Storage directory for type definition and doc resource files
(defconst eglot-luau-lsp-storage-location "~/.emacs.d/luau-lsp-storage/")

;; URL providing Roblox API docs
(defconst eglot-luau-lsp-roblox-docs-url "https://raw.githubusercontent.com/MaximumADHD/Roblox-Client-Tracker/roblox/api-docs/en-us.json")

;; URL providing latest Roblox version
(defconst eglot-luau-lsp-roblox-version-url "https://raw.githubusercontent.com/CloneTrooper1019/Roblox-Client-Tracker/roblox/version.txt")

;; Declare eglot variables to silence warnings
(defvar eglot-server-programs)

;; Whether the client has checked for updated Roblox types/docs
;; Used to prevent unnecessary HTTP requests
(defvar eglot-luau-lsp-has-not-checked-version t)

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

(provide 'eglot-luau-lsp-vars)
;;; eglot-luau-lsp-vars.el ends here
