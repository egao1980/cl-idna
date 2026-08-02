# Cl-Idna - CL-IDNA is a Internationalized Domain Names in Applications API

Portable library implementing IDNA2008 name translation according to <https://unicode.org/reports/tr46/>.

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

**OCI (cl-repository / cl-stack):**

```
ghcr.io/egao1980/cl-systems/cl-idna:0.1.0
```

```common-lisp
(asdf:load-system "cl-repository-client")
(cl-repository-client/quickload:add-registry "https://ghcr.io"
  :namespace "egao1980/cl-systems")
(cl-repo:load-system "cl-idna")
```

**Ultralisp:**

```common-lisp
;; install Ultralisp if you haven't done it yet
(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
(ql:quickload :cl-idna)
```

Not on Quicklisp — that is why stack consumers prefer the OCI pin (and why `egao1980/quri` is not proposed upstream yet).


## Author

* Nikolai Matiushev
* Andreas Fuchs

## Copyright

- Copyright (c) 2020 Nikolai Matiushev
- Copyright (c) 2011 Andreas Fuchs

## License

Licensed under the MIT License.
