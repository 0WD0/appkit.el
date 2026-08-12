;; -*- mode: eask; lexical-binding: t -*-

(package "appkit"
         "0.2.10"
         "Runtime primitives for stateful buffer applications")

(website-url "https://github.com/0WD0/appkit.el")
(keywords "lisp" "extensions")

(package-file "appkit.el")

(source "gnu")
(source 'melpa)

(depends-on "emacs" "27.1")
(depends-on "plz" "0.8")

(development
 (depends-on "evil"))

(script "test" "eask test ert --quiet test/*.el")
