# Thermal
The Robot Mind and Language


 {


 infixl 8 @ :
   [a b] . select [a b]

 (* my_rec@`foo` 44 *)

 infixr 0 ;= :
   [a f] => f a

 infixl 8 * :
   [a b] => mult [a b]

 fact :
   0 => 1 |
   n => n * (fact (n - 1)) 

 (* strings may be in back ticks *)
 (* lists do not have commas when within square braces *)

 even :
   0 => true |
   [#S n] => odd n

 odd :
   0 => false |
   [#S n] => even n

 (* strings without whitespace may use hash *)

 (* recursion defined within record syntax *)

 fib : 
   0 => 0 |
   1 => 1 |
   `S` , [`S` n] => fib [`S` n] + fib n 


 infix 8 = :
   [a b] => [a `=` b] 

 infixl 3 \/ :
   [sym a sym b] => [a `\/` b] 

 (* constraint logic precedence: EQUAL > NOT > AND > XOR > OR > EQUIV > IMP > *)

 (* prefix keyword allows using fib in prefix position *)
 (* comma used to construct list without square brackets*)

 } ; 

 4 ;= x . 

 (fact x) ;= y .
 
 fib y ; 

 solve x :: x = 4 \/ x = 2 for [`Answer` x]

 (* sym keyword directs arguments for param to be wrapped in thunk, rather than evaluated *)

 (* semi opens preceding record inside or simply calls following procedure *)
 (* semi-eq applies right side to argument on left *)

 (* records do not have commas when within curly braces *)



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
