(defpackage cl-idna
  (:use :cl
        ))
(in-package :cl-idna)

(defconstant +delimiter+ (code-char #x2d))
(defconstant +initial-n+ #x80)
(defconstant +initial-bias+ 72)
(defconstant +base+ 36)
(defconstant +maxint+ #x7FFFFFFF)
(defconstant +tmin+ 1)
(defconstant +tmax+ 26)
(defconstant +damp+ 700)
(defconstant +skew+ 38)
(defconstant +zero+ #x30)
(defconstant +small-a+ #x61)
(defconstant +small-z+ #x7a)
(defconstant +capital-a+ #x41)
(defconstant +capital-z+ #x5a)


(defun adapt (delta numpoints first-time)
  (setf delta (if first-time
                  (truncate delta +damp+)
                  (truncate delta 2)))
  (incf delta (truncate delta numpoints))
  (+ (do ((k 0 (+ k +base+)))
         ((<= delta (truncate (* (- +base+ +tmin+) +tmax+) 2)) k)
       (setf delta (truncate delta (- +base+ +tmin+))))
     (truncate (* (1+ (- +base+ +tmin+)) delta)
               (+ delta +skew+))))

(defun basic-p (c) (< c +initial-n+))

(defun basic-uppercase-p (c) (< +capital-a+ c +capital-z+))

(defun ascii-case-map (c flag)
  (let ((delta (- +small-a+ +capital-a+)))
    (cond ((and flag (< +small-a+ c +small-z+))
           (- c delta))
          ((and (not flag) (< +capital-a+ c +capital-z+))
           (- c delta))
          (t
           c))))

(defun encode-digit (digit flag)
  (code-char
   (+ digit
      (if (< digit 26)
          (if flag +capital-a+ +small-a+)
          +zero+))))


(defun punycode-encode (code-points &key case-flags)
  "Encode STRING with the punycode algorithm documented in RFC3492.
When PRESERVE-CASE is true, emit case annotations and do not perform
case folding (to downcase), as required for ToASCII."
  (let* ((input-length (length code-points))
         (h 0)
         (b 0)
         (m +maxint+)
         (n +initial-n+)
         (delta 0)
         (bias +initial-bias+)
         (encodedp))
    (values
     (with-output-to-string (output)
       (loop for c-code in code-points
             do (when (basic-p c-code)
                  (write-char (code-char(if case-flags
                                             (ascii-case-map c-code (nth h case-flags))
                                             c-code))
                              output)
                  (incf b)
                  (incf h)))
       ;; h is the number of code points that have been handled, b is
       ;; the number of basic code points.
       (when (and (> b 0) (< h input-length))
         (write-char +delimiter+ output))
       ;; Main encoding loop:
       (loop
         (unless (< h input-length)
           (return))
         (setf encodedp t)
         (setf m +maxint+)
         ;; All non-basic code points below n have been handled
         ;; already. Find the next larger one:
         (loop for c-code in code-points
               when (and (>= c-code n) (> m c-code))
                 do (setf m c-code))

         ;; Increase delta enough to advance the decoder's <n,i>
         ;; state to <m,0> but guard against overflow:
         (when (> (- m n) (truncate (- +maxint+ delta) (1+ h)))
           (error "punycode_overflow(1)"))

         (incf delta (* (- m n) (1+ h)))
         (setf n m)

         (loop for c-code in code-points
               do (when (and (< c-code n) (> (incf delta) +maxint+))
                    (error "punycode_overflow(2)"))
                  (when (= c-code n)
                    ;; represent delta as a generalized variable-length integer:
                    (let ((q (do* ((q delta (truncate (- q tee) (- +base+ tee)))
                                   (k +base+ (+ k +base+))
                                   (tee (if (<= k bias)
                                            +tmin+
                                            (if (>= k (+ bias +tmax+))
                                                +tmax+
                                                (- k bias)))
                                        (if (<= k bias)
                                            +tmin+
                                            (if (>= k (+ bias +tmax+))
                                                +tmax+
                                                (- k bias)))))
                                  ((< q tee) q)
                               (write-char (encode-digit (+ tee (rem (- q tee) (- +base+ tee))) nil)
                                           output))))
                      (write-char (encode-digit q (and case-flags (nth h case-flags))) output))
                    (setf bias (adapt delta (1+ h) (= h b)))
                    (setf delta 0)
                    (incf h)))
         (incf delta)
         (incf n)))
     encodedp)))

(defun idna-map (name &key transitional-processing (use-std3-ascii-rules t))
  (let ((+ignored+ (cl-unicode:property-symbol "ignored"))
        (+deviation+ (cl-unicode:property-symbol "deviation"))
        (+valid+ (cl-unicode:property-symbol "valid"))
        (+mapped+ (cl-unicode:property-symbol "mapped"))
        (+disallowed-std3-valid+ (cl-unicode:property-symbol "disallowed_STD3_valid"))
        (+disallowed-std3-mapped+ (cl-unicode:property-symbol "disallowed_STD3_mapped")))
    (loop for c across name
          for code-point = (char-code c)
          for mapping = (cl-unicode:idna-mapping code-point)
          for status = (car mapping)
          nconc (cond
                  ((or (eql status +valid+)
                       (and (not transitional-processing)
                            (eql status +deviation+))
                       (and (not use-std3-ascii-rules)
                            (eql status +disallowed-std3-valid+)))
                   (list code-point))
                  ((eql status +ignored+)
                   nil)
                  ((or
                    (and transitional-processing
                         (eql status +deviation+))
                    (eql status +mapped+)
                    (and (not use-std3-ascii-rules)
                         (eql status +disallowed-std3-mapped+)))
                   (nth 1 mapping))
                  (t
                   (error "Disallowed character"))))))
