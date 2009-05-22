;;;; -*- Lisp -*-

(defpackage :cl-paypal-system (:use #:asdf #:cl))

(in-package :cl-paypal-system)

(defsystem cl-paypal
  :name "cl-paypal"
  :version "0.1"
  :maintainer "Plato Wu <netawater@gmail.com>"
  :author "Hans Huebner <hans@huebner.org>, Plato Wu <netawater@gmail.com>"
  :licence "BSD"
  :description "A paypal express checkout API in Common Lisp."
  :depends-on (:hunchentoot :drakma :cl-ppcre)
  :components ((:file "package")
	      (:file "config" :depends-on ("package"))
	      (:file "cl-paypal" :depends-on ("config"))))
