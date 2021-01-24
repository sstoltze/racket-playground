#lang brag
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
gherkin : (/COMMENT | feature | /NEWLINE)+
feature : FEATURE  /NEWLINE ([scenario] /NEWLINE)* [background] /NEWLINE
title : STRING
scenario : SCENARIO /NEWLINE given /NEWLINE when /NEWLINE then /NEWLINE [examples]
given : GIVEN [/NEWLINE and]
when : WHEN [/NEWLINE and]
then : THEN [/NEWLINE and]
and : AND [/NEWLINE and]
background : BACKGROUND /NEWLINE [given] /NEWLINE [examples]
examples : EXAMPLES /NEWLINE variable-names /NEWLINE variable-values
variable-names : (/SEPARATOR variable)* /SEPARATOR
variable-values : (/SEPARATOR value)* /SEPARATOR [/NEWLINE variable-values]
variable : STRING+
value : (STRING | INTEGER)+
