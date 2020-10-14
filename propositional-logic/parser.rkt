#lang brag
@sentence     : biimplication
              | implication
              | disjunction
biimplication : disjunction /IFF disjunction
implication   : disjunction /IMPLIES disjunction
disjunction   : [disjunction /OR] conjunction
conjunction   : [@conjunction /AND] singleton
@singleton    : negation
              | var
negation      : /NOT var
@var          : atom | paren
@paren        : /LPAR @sentence /RPAR
atom          : ATOMIC
