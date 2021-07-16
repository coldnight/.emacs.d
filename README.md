<p align="center">
    <img src="https://raw.githubusercontent.com/coldnight/.emacs.d/master/logo.png" alt="Emacs Configuration" />
</p>

## Screenshot

[![Screenshot](https://gist.githubusercontent.com/coldnight/85f0ffc797d17754cdb20a93b5536e69/raw/6114fe94c4d8aa42cae8a946ee7a7a6eacde4b31/emacs.png)](https://gist.githubusercontent.com/coldnight/85f0ffc797d17754cdb20a93b5536e69/raw/6114fe94c4d8aa42cae8a946ee7a7a6eacde4b31/emacs.png)

## About

My Emacs configuration that includes:

- Manage package via `use-package` & `straight`
- Python/Go/LSP
- Org-mode/org-roam/org-journal

## Installation

```shell
$ git clone https://github.com/coldnight/.emacs.d ~/.emacs.d
$ git submodule init
$ git submodule update --init
```

Open your Emacs and wait initialize done.

### Optional

 You can configure Git to connect to GitHub over SSH, if you have issues to connect to GitHub with HTTPs.

```shell
$ git config --global url."git@github.com:".insteadOf "https://github.com/"
```

## Setup


### icons

Install all-the-icons fonts: `M-x all-the-icons-install-fonts`

### org-roam & org-journal & org-agenda &ox-hugo

1. [Install hugo](https://gohugo.io/getting-started/installing/)
2. Create a hugo site

	```shell
	$ hugo new site ~/codes/notes/roam-research-notes-hugo
	```
3. Make directories

	```shell
	$ mkdir -p ~/codes/notes/roam-research-notes-hugo/journal
	$ mkdir -p ~/codes/notes/roam-research-notes-hugo/content-org
	$ mkdir -p ~/codes/notes/roam-research-notes-hugo/gtd
	```

### Golang

Install gopls

```shell
GO111MODULE=on go get golang.org/x/tools/gopls@latest
```

Set GOPROXY:

``` shell
go env -w GOPROXY="https://goproxy.io,direct"
```

### Graphviz

[Install Graphviz](https://graphviz.org/download/) to draw it in org-mode with evaluate code block.

### C/C++

Install [clangd](https://clangd.llvm.org/installation.html) for `lsp-mode`.

Use cmake to generate clangd configurations:

``` shell
mkdir build
cd build
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..
mv compile_commands.json ../
```
### Rust

[Install rust-analyzer](https://rust-analyzer.github.io/manual.html#rust-analyzer-language-server-binary).

### Notify

``` shell
brew install terminal-notifier
```


# Awesome .emacs.d

- https://tecosaur.github.io/emacs-config/
- https://github.com/lijigang/emacs.d
