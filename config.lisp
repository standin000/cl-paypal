(in-package :cl-paypal)

(defparameter *paypal-url* nil
  "NVP URL of the Paypal server")
(defparameter *paypal-user* nil
  "Username to use to authenticate at the Paypal server")
(defparameter *paypal-password* nil
  "Password to use to authenticate at the Paypal server")

(defparameter *paypal-signature* nil
  "Signature to use to authenticate at the Paypal server")

(defparameter *paypal-return-url* "return-paypal"
  "return url after finish paypal express checkout"
  )

(defparameter *paypal-return-url* nil
  "return url for finishing paypal express checkout"
  )

(defparameter *paypal-cancel-url* nil
  "cancel url if cancel paypal express checkout"
  )

(defun init (paypal-url paypal-user paypal-password paypal-signature
	     paypal-return-url paypal-cancel-url)
  (setf *paypal-url* paypal-url
	*paypal-user* paypal-user
	*paypal-password* paypal-password
	*paypal-signature* paypal-signature
	*paypal-return-url* paypal-return-url
	*paypal-cancel-url* paypal-cancel-url
	)
  )
