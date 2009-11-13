(in-package :cl-paypal)

(defvar *paypal-api-url* nil
  "NVP URL of the Paypal server")
(defvar *paypal-user* nil
  "Username to use to authenticate at the Paypal server")
(defvar *paypal-password* nil
  "Password to use to authenticate at the Paypal server")

(defvar *paypal-signature* nil
  "Signature to use to authenticate at the Paypal server")

(defvar *paypal-return-url* nil
  "return url for finishing paypal express checkout")

(defvar *paypal-cancel-url* nil
  "cancel url if cancel paypal express checkout")

(defvar *paypal-useraction* nil
  "confirm order on your site or on Paypal, continue or commit, respectively")

(defvar *paypal-currencycode* nil
  "currency for paypal express checkout")

(defun init (paypal-api-url paypal-user paypal-password paypal-signature
	     paypal-return-url paypal-cancel-url &key (useraction "continue") (currencycode "USD"))
  (setf *paypal-api-url* paypal-api-url
	*paypal-user* paypal-user
	*paypal-password* paypal-password
	*paypal-signature* paypal-signature
	*paypal-return-url* paypal-return-url
	*paypal-cancel-url* paypal-cancel-url
	*paypal-useraction* useraction
	*paypal-currencycode* currencycode
	))

