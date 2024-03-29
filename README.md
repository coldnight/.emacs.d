
## Screenshot

[![Screenshot](https://gist.githubusercontent.com/coldnight/85f0ffc797d17754cdb20a93b5536e69/raw/18d403d216f254d66b2ebd9687b083728766047d/emacs.png)](https://gist.githubusercontent.com/coldnight/85f0ffc797d17754cdb20a93b5536e69/raw/18d403d216f254d66b2ebd9687b083728766047d/emacs.png)

## About
<p align="center">
    <img src="https://raw.githubusercontent.com/coldnight/.emacs.d/master/logo.png" alt="Emacs Configuration" />
</p>

My Emacs configuration that includes:

- Manage package via `use-package` & `straight`
- Python/Go/LSP
- Org-mode/org-roam/org-journal

## Installation

```shell
$ git clone https://github.com/coldnight/.emacs.d ~/.emacs.d
```

Open your Emacs and wait the initialization done.

### Optional

 You can configure Git to connect to GitHub over SSH, if you encountered network issues when connected to GitHub over HTTPs.

```shell
$ git config --global url."git@github.com:".insteadOf "https://github.com/"
```

## Setup

### Fonts

- <s>[Victor Mono](https://github.com/rubjo/victor-mono)</s>
- [MonoLisa Nasy](https://www.monolisa.dev/)
- <s>STKaiti（macOS 华文楷体）</s>
- [文泉驿正黑（包含等宽）](http://wenq.org/wqy2/index.cgi?Download#ZenHei_Stable)

### icons

Install all-the-icons fonts: `M-x all-the-icons-install-fonts RET`

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

``` shell
cargo install cargo-add cargo-expand cargo-watch
```

`M-x lsp-install-server RET rust-analyzer RET`

### Notify

``` shell
brew install terminal-notifier
```
### vterm

``` shell
brew install cmake libvterm
```

# Awesome .emacs.d

- https://tecosaur.github.io/emacs-config/
- https://github.com/lijigang/emacs.d
