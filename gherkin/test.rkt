#lang gherkin
Feature: User Log In
  Scenario: Successful Registration
  Given there is no account with username <username>
  And there is no account with email <email>
  When I register the account with username <username>, email <email> and password <password>
  Then it should successfully create the account with <username>, <email>, and <password>
  Examples:
    | username | email              | password      |
    | john doe | john@doe.com       | ABCD1234!?    |
    | jane doe | jane.doe@gmail.com | abcdefgh1.aba |
    | jackson  | jackson@yahoo.com  | cadsw4ll0p/   |

Background:
  Given: There is an existing user with username <username>, email <email> and password <password>
  Examples:
    | username | email              | password      |
    | john doe | john@doe.com       | ABCD1234!?    |
    | jane doe | jane.doe@gmail.com | abcdefgh1.aba |
