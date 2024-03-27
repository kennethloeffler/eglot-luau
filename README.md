# eglot-luau

eglot-luau is an Emacs package that adds support for the [Luau Lanuage Server](https://github.com/JohnnyMorganz/luau-lsp) to [eglot](https://github.com/joaotavora/eglot).

## Configuration

The entry point to the package is the function `eglot-luau-setup`. This function should be called after eglot is loaded.

eglot-luau provides the following customizable variables:

* `eglot-luau-rojo-sourcemap-enabled`

* `eglot-luau-rojo-sourcemap-includes-non-scripts`

* `eglot-luau-auto-update-roblox-types`

* `eglot-luau-auto-update-roblox-docs`

* `eglot-luau-roblox-security-level`

* `eglot-luau-rojo-project-path`

* `eglot-luau-fflags-enabled`

* `eglot-luau-fflag-overrides`

* `eglot-luau-sync-fflags`

* `eglot-luau-custom-type-files`

* `eglot-luau-custom-doc-files`

* `eglot-luau-server-executable`

eglot-luau is also configurable using LSP's "workspace configuration," as described in [the Eglot documentation](https://joaotavora.github.io/eglot/#Project_002dspecific-configuration-1).

Below is a sample configuration using `use-package`:

```elisp
(use-package eglot-luau
  :demand t
  :after (lua-mode eglot)
  :functions eglot-luau-setup
  :config (eglot-luau-setup)
  :custom
  (eglot-luau-rojo-sourcemap-enabled t)
  (eglot-luau-rojo-sourcemap-includes-non-scripts t)
  (eglot-luau-auto-update-roblox-docs t)
  (eglot-luau-auto-update-roblox-types t)
  (eglot-luau-server-executable "~/projects/programming/luau-lsp/build/luau-lsp")
  (eglot-luau-fflag-overrides '(("LuauNonStrictByDefaultBetaFeature" "False")))
  :hook (lua-mode . eglot-ensure))
```

## Known issues

`end` completion does not work. This seems to be a deeper issue in eglot that causes `\n` to be unusable as a trigger character. Since luau-lsp is hardcoded to use `\n` as a trigger character for `end` completion, this is not very straightforward to fix.
