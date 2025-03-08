(defpackage cl-idna
  (:use :cl)
  (:export #:to-ascii #:to-unicode))
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

(defun before-init ()
  "Is run when the system is loaded"
  (pushnew :idna-unicode *features*))

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
          (- +zero+ 26)))))


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
                  (write-char (code-char (if case-flags
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
                                   (tt (- k bias) (- k bias))
                                   (tee (cond ((< tt +tmin+)
                                               +tmin+)
                                              ((>= k (+ bias +tmax+))
                                               +tmax+)
                                              (t tt))
                                        (cond ((< tt +tmin+)
                                               +tmin+)
                                              ((>= k (+ bias +tmax+))
                                               +tmax+)
                                              (t tt))))
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


(defun decode-digit (cp)
  (cond ((< (- cp 48) 10) (- cp 22))
        ((< (- cp 65) 26) (- cp 65))
        ((< (- cp 97) 26) (- cp 97))
        (t +base+)
        ))

(defun punycode-decode (code-points)
  (declare (optimize debug))
  (let ((output nil)
        (output-length 0)
        (n +initial-n+)
        (i 0)
        (bias +initial-bias+)
        (basic (or (position +delimiter+ code-points :from-end t)
                   0))
        (oldi 0))

    (loop for char-code in (subseq code-points 0 basic)
          do (when (>= char-code #x80)
               (error "Invalid input ~x >=#x80" char-code))
             (setf output (nconc output (list char-code)))
             (incf output-length))

    ;; Main decoding loop: Start just after the last delimiter if any
    ;; basic code points were copied; start at beginning otherwise.

    (loop with input = (if (zerop basic) code-points (subseq code-points (1+ basic)))
          with out = 0
          while input
          ;; ic is the index of the next character to be consumed.
          ;; Decode a generalized variable-length integer into delta,
          ;; which gets added to i. The overflow checking is easier if
          ;; we increase i as we go, then subtract off its starting
          ;; value at the end to obtain delta.
          do (setf oldi i)
             (loop with w = 1
                   with digit
                   with tee
                   for k from +base+ by +base+
                   do
                      (assert input () "punycode_bad_input(1)")
                      (setf digit (decode-digit (car input)))
                      (setf input (rest input))
                      (setf tee (cond
                                  ((<= k bias) +tmin+)
                                  ((>= k (+ bias +tmax+)) +tmax+)
                                  (t (- k bias))))
                      (assert (< digit +base+) () "punycode_bad_input(2)")
                      (assert (< digit (truncate (- +maxint+ i) w)) () "punycode_overflow(1)")
                      (incf i (* digit w))
                      (when (< digit tee) (return))
                      (assert (<= w (truncate +maxint+ (- +base+ tee))) () "punycode_overflow(2)")
                      (setf w (* w (- +base+ tee))))
             (setf out (1+ output-length))
             (setf bias (adapt (- i oldi) out (zerop oldi)))

             ;; i was supposed to wrap around from out to 0,
             ;; incrementing n each time, so we'll fix that now:
             (assert (< (truncate i out) (- +maxint+ n)) () "punycode_overflow(3)")
             (incf n (truncate i out))
             (setf i (rem i out))

             (setf output (nconc (subseq output 0 i) (list n) (subseq output i)))
             (incf output-length)
             (incf i))
    output))

(defun idna-map (name &key transitional-processing-p (use-std3-ascii-rules-p t))
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
          unless (eql status +ignored+)
          collect (cond
                    ((or (eql status +valid+)
                         (and (not transitional-processing-p)
                              (eql status +deviation+))
                         (and (not use-std3-ascii-rules-p)
                              (eql status +disallowed-std3-valid+)))
                     code-point)
                    ((or (and transitional-processing-p
                              (eql status +deviation+))
                         (eql status +mapped+)
                         (and (not use-std3-ascii-rules-p)
                              (eql status +disallowed-std3-mapped+)))
                     (caadr mapping))
                    (t
                     (error "Disallowed character"))))))

(defun check-label (code-points &key transitional-processing-p
                                     (use-std3-ascii-rules-p t)
                                     (check-hyphens-p t)
                                     (check-bidi-p t)
                                     (check-joiners-p t))
  (let* ((punycoded-p (when (> (length code-points) 3)
                        (equal '(#x78 #x6e #x2d #x2d) (subseq code-points 0 4))))
         (label (if punycoded-p (punycode-decode (subseq code-points 4)) code-points)))
    (when (not (equal label (cl-unicode:normalization-form-c label)))
      (error "The label must be in Unicode Normalization Form NFC."))
    (let ((minus-char (char-code #\-)))
      (when (and check-hyphens-p
                 (find minus-char (list (car label)
                                        (nth 2 label)
                                        (nth 3 label)
                                        (car (last label)))))
        (error "The label must not contain a U+002D HYPHEN-MINUS character in the first, third, fourth and the last positions.")))
    (when (find (char-code #\.) label)
      (error "The label must not contain a U+002E ( . ) FULL STOP."))
    (when (string= "Mark" (cl-unicode:general-category (car label)))
      (error "The label must not begin with a combining mark."))
    (let ((+deviation+ (cl-unicode:property-symbol "deviation"))
          (+valid+ (cl-unicode:property-symbol "valid"))
          (+disallowed-std3-valid+ (cl-unicode:property-symbol "disallowed_STD3_valid")))
      (when (find-if #'(lambda (c)
                         (let ((status (car (cl-unicode:idna-mapping c))))
                           (not (or (and (not use-std3-ascii-rules-p)
                                         (eql status +disallowed-std3-valid+))
                                    (if (or (not transitional-processing-p) punycoded-p)
                                        (or (eql status +valid+)
                                            (eql status +deviation+))
                                        (eql status +valid+))))))
                     label)
        (error "The label contains a character with invalid status.")))
    (values code-points label)))


(defun prepare (name &key transitional-processing-p (use-std3-ascii-rules-p t))
  (cl-unicode:normalization-form-c (idna-map name
                                             :transitional-processing-p transitional-processing-p
                                             :use-std3-ascii-rules-p use-std3-ascii-rules-p)))

(defun to-ascii (string &key transitional-processing-p
                             (use-std3-ascii-rules-p t)
                             (check-hyphens-p t)
                             (check-bidi-p t)
                             (check-joiners-p t))
  "Encode string to IDNA punycode format using the ToASCII algorithm."
  (with-output-to-string (output)
    (loop for (component . rest) on (cl-utilities:split-sequence (char-code #\.) (prepare string))
          do (multiple-value-bind (punycode encodedp) (punycode-encode
                                                       (check-label component
                                                                    :transitional-processing-p transitional-processing-p
                                                                    :use-std3-ascii-rules-p use-std3-ascii-rules-p
                                                                    :check-hyphens-p check-hyphens-p
                                                                    :check-bidi-p check-bidi-p
                                                                    :check-joiners-p check-joiners-p))
               (cond (encodedp
                      (write-string "xn--" output)
                      (write-string punycode output))
                     (t (write-sequence (mapcar #'code-char component) output)))
               (when rest
                 (write-char #\. output))))))

(defun to-unicode (string &key transitional-processing-p
                               (use-std3-ascii-rules-p t)
                               (check-hyphens-p t)
                               (check-bidi-p t)
                               (check-joiners-p t))
  "Encode string to IDNA punycode format using the ToASCII algorithm."
  (with-output-to-string (output)
    (loop for (component . rest) on (cl-utilities:split-sequence (char-code #\.) (prepare string))
          do (multiple-value-bind (raw decoded) (check-label component
                                                             :transitional-processing-p transitional-processing-p
                                                             :use-std3-ascii-rules-p use-std3-ascii-rules-p
                                                             :check-hyphens-p check-hyphens-p
                                                             :check-bidi-p check-bidi-p
                                                             :check-joiners-p check-joiners-p)
               (declare (ignore raw))
               (write-sequence (mapcar #'code-char decoded) output)
               (when rest
                 (write-char #\. output))))))
