#lang typed/racket/base

(struct None ())
(struct (a) Some ([v : a]))

(define-type (Opt a) (U None (Some a)))

;; Feature: User Log In
;;   Scenario: Successful Registration
;;   Given There is no account with username <username>
;;     or email <email>
;;   When I register the account with username <username>,
;;     email <email> and password <password>
;;   Then it should successfully create the account
;;     with <username>, <email>, and <password>
;;   Examples:
;;     | username | email              | password      |
;;     | john doe | john@doe.com       | ABCD1234!?    |
;;     | jane doe | jane.doe@gmail.com | abcdefgh1.aba |
;;     | jackson  | jackson@yahoo.com  | cadsw4ll0p/   |
;;
;; Background:
;;   Given: There is an existing user with username <username>,
;;     email <email> and password <password>
;;   Examples:
;;     | username | email              | password      |
;;     | john doe | john@doe.com       | ABCD1234!?    |
;;     | jane doe | jane.doe@gmail.com | abcdefgh1.aba |

(struct feature ([title       : String]
                 [description : (Listof String)]
                 [background  : (Opt scenario)]
                 [scenarios   : (Listof scenario)]))

(struct scenario ([title      : String]
                  [statements : (Listof statement)]
                  [examples   : example-table]))

(struct example-table ([keys     : (Listof String)]
                       [examples : (Listof (Pair String Any))]))

(struct statement ([text      : String]
                   [variables : (Listof String)]))
