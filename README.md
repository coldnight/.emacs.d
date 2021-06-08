<p align="center">
    <img src="https://raw.githubusercontent.com/coldnight/.emacs.d/master/logo.png" alt="Emacs Configuration" />
</p>

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
