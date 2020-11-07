# Cl-Idna - CL-IDNA is a Internationalized Domain Names in Applications API

Portable library implementing IDNA2008 name translation according to [[https://unicode.org/reports/tr46/]].

## Usage

Encoding strings as IDNA:

     (cl-idna:to-ascii "中央大学.tw")
     ;; => "xn--fiq80yua78t.tw"

     (cl-idna:to-ascii "βόλος.com")
     ;; => "xn--nxasmm1c.com"

     (cl-idna:to-ascii "ශ්‍රී.com")
     ;; => "xn--10cl1a0b660p.com"

     (cl-idna:to-ascii "نامه‌ای.com")
     ;; => "xn--mgba3gch31f060k.com"


Decoding strings from IDNA notation to unicode text:


     (cl-idna:to-unicode "xn--mgba3gch31f060k.com")
     ;; => "نامه‌ای.com"


## Installation

CL-IDNA is available at [[Ultralisp.org]]

      ;; install Ultralisp if you haven't done it yet
      (ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
      ;; install CL-IDNA
      (ql:quickload :cl-idna)


## Author

* Nikolai Matiushev
* Andreas Fuchs

## Copyright

- Copyright (c) 2020 Nikolai Matiushev
- Copyright (c) 2011 Andreas Fuchs

## License

Licensed under the MIT License.
