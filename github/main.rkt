#lang racket

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require dotenv
         github-api
         net/base64)
(dotenv-load!)

(define gh-id (github-identity 'personal-token (list (getenv "GITHUB_USERNAME") (getenv "GITHUB_PERSONAL_ACCESS_TOKEN"))))
(define github (github-api gh-id))

(define (get-repos owner)
  (github-response-data (github (format "/orgs/~A/repos" owner))))

(define (get-repo owner repo)
  (github-response-data (github (format "/repos/~A/~A" owner repo))))

(define (get-codeowners . repo)
  (define codeowners-url (match repo
                           [(list (hash-table ('contents_url u))) (string-replace u "{+path}" "CODEOWNERS")]
                           [(list owner repo) (format "/repos/~A/~A/contents/CODEOWNERS" owner repo)]))
  (define encoded-content (hash-ref (github-response-data (github codeowners-url))
                                    'content))
  (bytes->string/utf-8 (base64-decode (string->bytes/utf-8 encoded-content))))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline)
  (define who (box "world"))
  (command-line
    #:program "my-program"
    #:once-each
    [("-n" "--name") name "Who to say hello to" (set-box! who name)]
    #:args ()
    (printf "hello ~a~n" (unbox who))))
