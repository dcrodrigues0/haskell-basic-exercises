module Ex1 where

-- 3.1 Create type Answer and some functions 
data Answer = Yes | No

getAnswerNum Yes = 1
getAnswerNum No  = 0

listAnswers answers = [ getAnswerNum(answer) | answer <- answers ]

and' (Yes, Yes)  = True
and' (No, No)    = False
and' (Yes, No)   = False
and' (No, Yes)   = False

or'  (Yes, Yes)  = True
or'  (No, No)    = False
or'  (Yes, No)   = True
or'  (No, Yes)   = True

not' (Yes, Yes)  = False
not' (No, No)    = True
not' (Yes, No)   = False
not' (No, Yes)   = False