# Thermal
The Robot Mind and Language

P --> Q
~ P \\/ Q
~ (P /\\ ~ Q)  

not P \\/ Q
not (P /\\ not Q)  

not P or Q
not (P and not Q)  


(for x # P x /\\\\ Q x) reduces to (for x # P x /\ ~ Q x)
(for x # P x --> Q x) reduces to ~ (for x # P x /\ ~ Q x)

(for x # P x --> Q x --> R x) reduces to
~ (for x # P x /\\ ~ (Q x --> R x)) reduces to
~ (for x # P x /\\ Q x /\\ ~ R x)

** "sans" may only have free variables in latter term if
they are present in former term

** "not" may have free variables in term only if 
those free variable are present in positive formula in preceding conjuction
and preceding formula is well-formed.


the modes "reduced", "called", "returned", "spawned", "blocked", "synced", "stuck", and "done" are higher order predicates that take a predicate as an argument where the body of the argument is evaluated at the next step is the next step, if the step taken is that described by the mode. "sent" and "recved" may be defined in terms of "synched" by checking that the current thread corresponds to the current end of the sync transaction.  
The current thread's id is retrieved using keyword "this" 
