# eglot-luau [![MELPA](https://melpa.org/packages/eglot-luau-badge.svg)](https://melpa.org/#/eglot-luau)

eglot-luau is an Emacs package that adds support for the [Luau Lanuage Server](https://github.com/JohnnyMorganz/luau-lsp) to [eglot](https://github.com/joaotavora/eglot).

## Configuration

eglot-luau provides the following customizable variables:

* `eglot-luau-rojo-sourcemap-enabled`: `boolean`

	This variable determines whether eglot-luau will start a Rojo process to generate sourecemaps for the language server. The value of this variable is `nil` by default.

* `eglot-luau-rojo-sourcemap-includes-non-scripts`: `boolean`

	This variable determines whether non-script instances are present in the Rojo sourcemap. The value of this variable is `nil` by default.

* `eglot-luau-auto-update-roblox-types`: `boolean`

	This variable determines whether `eglot-luau-setup` will attempt to download the latest Roblox type definitions. The value of this variable is `nil` by default.

* `eglot-luau-auto-update-roblox-docs`: `boolean`

	This variable determines whether `eglot-luau-setup` will attempt to download the latest Roblox docs. The value of this variable is `nil` by default.

* `eglot-luau-roblox-security-level`: `string`

	This variable determines the security level of type definitions downloaded when `eglot-luau-lsp-auto-update-roblox-types` is non-`nil`. The available options are `None`, `LocalUserSecurity`, `PluginSecurity`, and `RobloxScriptSecurity`. The value of this variable is `PluginSecurity` by default.

* `eglot-luau-rojo-project-path`: `string`

	This variable determines the path passed to Rojo for sourcemap generation. The value of this variable is `default.project.json` by default.

* `eglot-luau-fflags-enabled`: `boolean`

	This variable determines whether the language server will have any FFlags enabled. The value of this variable is `t` by default.

* `eglot-luau-fflag-overrides`: `alist`

	This variable allows overriding FFlags. It is an alist mapping the names of FFlags to their desired values. An example value is ``(("LuauNonStrictByDefaultBetaFeature" "False"))`. The value of this variable is the empty list by default.

* `eglot-luau-sync-fflags`: `boolean`

	This variable determines whether `eglot-luau-setup` will attempt to download the latest Roblox FFlag configuration. The value of this variable is `t` by default.

* `eglot-luau-custom-type-files`: `list`

	This variable is a list of paths to custom type definition files to supply to the language server. The value of this variable is the empty list by default.

* `eglot-luau-custom-doc-files`: `list`

	This variable is a list of paths to custom doc files to supply to the language server. The value of this variable is the empty list by default.

* `eglot-luau-server-executable`: `string`

	This variable determines how to locate the language server. The value of this variable is `luau-lsp` by default (i.e. it assumes luau-lsp is on the PATH environment variable).

eglot-luau is also configurable using LSP's "workspace configuration," as described in [the Eglot documentation](https://joaotavora.github.io/eglot/#Project_002dspecific-configuration-1).

The entry point to the package is the function `eglot-luau-setup`. This function should be called after eglot is loaded, but before eglot is invoked, once per major mode you intend to use eglot-luau. A good time to do this is in major mode hooks. It takes one argument: the symbol of the mode for which to setup eglot-luau.

Below is a example configuration using `use-package`:

```elisp
(use-package eglot-luau
  :autoload (eglot-luau-setup)
  :custom
  (eglot-luau-rojo-sourcemap-enabled t)
  (eglot-luau-rojo-sourcemap-includes-non-scripts t)
  (eglot-luau-auto-update-roblox-docs t)
  (eglot-luau-auto-update-roblox-types t)
  (eglot-luau-fflag-overrides '(("LuauSolverV2" "True")))
  :hook
  (lua-mode . (lambda () (eglot-luau-setup 'lua-mode)))
  (lua-mode . eglot-ensure))
```

## Known issues

`end` completion does not work. This seems to be caused by a deeper issue in eglot that causes `\n` to be unusable as a trigger character. Since luau-lsp is hardcoded to use `\n` as a trigger character for `end` completion, this is not very straightforward to fix.

Additionally, eglot-luau does not support bytecode generation, not does it support luau-lsp's Roblox Studio companion plugin.
