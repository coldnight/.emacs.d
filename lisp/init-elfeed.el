;;; init-elfeed.el -- 配置 elfeed
;;;
;;; Commentary:
;;;
;;; Code:

(require 'elfeed)

(global-set-key (kbd "C-c w w") 'elfeed)
(global-set-key (kbd "C-c w u") 'elfeed-update)
(setq elfeed-feeds
      '("http://www.snarky.ca/feed"
        "http://www.linuxzen.com/feeds/all.atom.xml"
        "http://feeds.feedburner.com/DougHellmann"
        "http://eleveni386.7axu.com/feeds/rss.xml"
        "http://githubengineering.com/atom.xml"
        "https://news.ycombinator.com/rss"
        "https://rss.lilydjwg.me/zhihuzhuanlan/hackers"
        "http://www.yinwang.org/atom.xml"
        "http://blog.ionelmc.ro/feeds/all.atom.xml"
        "https://blog.jessfraz.com/index.xml"
        ("http://feeds.lepture.com/lepture" Python)
        "http://feeds.feedburner.com/linuxtoy"
        ("http://planet.emacsen.org/atom.xml" Emacs)
        ("http://www.solidot.org/index.rss" TechNews)
        ("http://www.vimer.cn/feed" Vim)
        "https://vsupalov.com/index.xml"
        "http://blog.lilydjwg.me/posts.rss"
        "http://coolshell.cn/feed"
        "http://feeds.feedburner.com/ruanyifeng"
        ("http://xargin.com/rss/" Go)
        ("https://colobu.com/atom.xml" Go)
        "http://lucumr.pocoo.org/feed.atom"
        ("http://love-python.blogspot.com/feeds/posts/default" Python)
        "http://online.effbot.org/rss.xml"
        ("http://planetpython.org/rss20.xml" Python)
        ("http://pyhome.org/rss/" Python)
        "http://feeds.memect.com/py.rss.xml"
        ("http://inventwithpython.com/blog/feed/" Python)
        ("http://www.blog.pythonlibrary.org/feed/" Python)
        ("http://xiaorui.cc/category/python/feed/" Python)
        ("https://hexilee.me/feed.xml" Rust)
        ("https://rss.lilydjwg.me/zhihuzhuanlan/rust-lang" Rust)
        ("http://this-week-in-rust.org/atom.xml" Rust)))

(if (boundp 'secret-github-feed)
    (push secret-github-feed elfeed-feeds))
(provide 'init-elfeed)
