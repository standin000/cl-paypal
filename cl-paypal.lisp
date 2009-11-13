(in-package :cl-paypal)

(define-condition paypal-error (error)
  ())

(define-condition request-error (paypal-error)
  ())

(define-condition http-request-error (paypal-error)
  ((http-status :initarg :http-status)
   (response-string :initarg :response-string)))

(define-condition response-error (paypal-error)
  ((response :initarg :response)
   (invalid-parameter :initarg :invalid-parameter)))

(define-condition transaction-already-confirmed-error (response-error)
  ())

(defun decode-response (response)
  "Decode a paypal response string, which is URL encoded and follow
  list encoding rules.  Returns the parameters as a plist."
  (let ((hash (make-hash-table)))
    (dolist (entry (cl-ppcre:split "&" response))
      (destructuring-bind (parameter-string value) (cl-ppcre:split "=" entry :limit 2)
        (multiple-value-bind (match registers) (cl-ppcre:scan-to-strings "^L_(.*?)([0-9]+)$" parameter-string)
          (if match
              (let* ((parameter (intern (aref registers 0) :keyword))
                     (index (parse-integer (aref registers 1)))
                     (previous-value (gethash parameter hash)))
                (unless (= (length previous-value) index)
                  (error 'response-error :invalid-parameter parameter-string :response response))
                (setf (gethash parameter hash) (append previous-value (list (hunchentoot:url-decode value :utf-8)))))
              (setf (gethash (intern parameter-string :keyword) hash) (hunchentoot:url-decode value :utf-8))))))
    (loop for key being the hash-keys of hash
         collect key
         collect (gethash key hash))))

(defun request (method &rest args &key &allow-other-keys)
  "Perform a request to the Paypal NVP API.  METHOD is the method to
  use, additional keyword arguments are passed as parameters to the
  API.  Returns "
  (multiple-value-bind (response-string http-status)
      (drakma:http-request *paypal-url*
                           :method :post
                           :parameters (append (list (cons "METHOD" method)
                                                     (cons "VERSION" "52.0")
                                                     (cons "USER" *paypal-user*)
                                                     (cons "PWD" *paypal-password*)
                                                     (cons "SIGNATURE" *paypal-signature*)
						     )
                                               (loop for (param value) on args by #'cddr
                                                  collect (cons (symbol-name param)
                                                                (if (stringp value)
                                                                    value
                                                                    (princ-to-string value))))))
    (unless (= 200 http-status)
      (error 'http-request-error :http-status http-status :response-string response-string))
    (let ((response (decode-response response-string)))
      (unless (string-equal "Success" (getf response :ack))
        (if (equal "10415" (car (getf response :errorcode)))
          (error 'transaction-already-confirmed-error :response response)
          (error 'response-error :response response)))
      response)))

(defvar *checkout-amount* 0 
  "store checkout amount between make-express-checkout-url and get-and-do-express-checkout")

(defun make-express-checkout-url (amount 
				  &optional 
				  (return-url *paypal-return-url*)
				  (cancel-url *paypal-return-url*)
				  )
  (let* ((amt (format nil "~,2F" amount))
	 (token (getf (request "SetExpressCheckout"
                               :amt amt
       ;; currencycode is not supported by SetExpressCheckoutPayment
                               :returnurl return-url
                               :cancelurl cancel-url
                               :paymentaction "Sale")
                      :token)))
    (setf *checkout-amount* amt)
    (format nil "https://www.sandbox.paypal.com/webscr?cmd=_express-checkout&token=~A~
                                     &AMT=~A&RETURNURL=~A&CANCELURL=~A"
             (hunchentoot:url-encode token)
             amt
             (hunchentoot:url-encode return-url)
             (hunchentoot:url-encode cancel-url))
    ))

(defun get-and-do-express-checkout (success failure)
  (with-output-to-string (*standard-output*)
    (let* ((token (hunchentoot:get-parameter "token"))
	   (response (request "GetExpressCheckoutDetails" :token token))
	   (payerid (getf response :payerid))
	   (result (request "DoExpressCheckoutPayment"
				      :token token
				      :payerid payerid
				      ;; amt is not returned by GetExpressCheckoutDetails 
				      ;; currencycode is not supported by DoExpressCheckoutPayment
				      :amt *checkout-amount*
				      :paymentaction "Sale"))
	   )
      (if (string-equal "Success" (getf result :ack))
	  (apply success '())
	  (apply failure '())
	  )
      )))