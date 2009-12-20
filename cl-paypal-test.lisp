
(eval-when (:compile-toplevel)
 (require :cl-paypal))

(setf hunchentoot:*show-lisp-errors-p* t)

(defgeneric dispatch-request (request-type request)
  (:documentation "dispatch incoming http request"))

(defmethod no-applicable-method ((function (eql #'dispatch-request)) &rest args)
  (declare (ignore args))
  nil)

(defmacro define-handler (type (request) &body body)
  (let ((request-type-var (gensym)))
    `(defmethod dispatch-request ((,request-type-var (eql ,type)) ,request)
       (declare (ignore ,request-type-var))
                                        ; (lambda () ,@body))))
       ,@body)))

(define-handler :checkout (request)
  (hunchentoot:redirect 
   (cl-paypal:make-express-checkout-url 1 (hunchentoot:remote-addr request))))

(define-handler :stop (request)
  (throw 'stop-server nil))


(define-handler :return-paypal (request)
  (cl-paypal:get-and-do-express-checkout
   (lambda (&key amount currencycode token result) 
     (format t "Paypal Express Checkout OK~%Amount is ~A~%
              Currencycod is ~A~%Token is~A~%Result is ~A"
             amount currencycode token result))
   (lambda () (print "Paypal Express Checkout NG"))))

(define-handler :cancel-paypal (request)
  "Cancelled")

(defun dispatch-request% (request)
  (let* ((type-string 
          (cl-ppcre:scan-to-strings "[^/]+" (hunchentoot:script-name request)))
         (request-type 
          (and type-string (find-symbol (string-upcase type-string) :keyword))))
    (dispatch-request request-type request)))

;; send-user "wangyi_1228286489_per@yeah.net" 
;; send-user's password "228286734"

(defun test-express-checkout (&key (response-port 8080) (response-host "192.168.1.24"))
  (cl-paypal:init "https://api-3t.sandbox.paypal.com/nvp" 
		"hans.huebner_api1.gmail.com"
		"62QFQPLEMM6P3M25"
		"AFcWxV21C7fd0v3bYYYRCpSSRl31Ac-RAs1SuG20a1IoPMJ0WKbx0fdG"
		(format nil "http://~A:~A/return-paypal" response-host response-port)
		(format nil "http://~A:~A/cancel-paypal" response-host response-port)
		:useraction "commit"
		:currencycode "EUR")
  (catch 'stop-server
    (hunchentoot:start (make-instance 'hunchentoot:acceptor :port response-port
                                      :REQUEST-DISPATCHER  #'dispatch-request%))))
