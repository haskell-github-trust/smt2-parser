; Axiom for list append: version 2
(forall ((l1 (List Int)) (l2 (List Int)))
  (= (append l1 l2)
  (match l1 (
    ((cons h t) (cons h (append t l2)))
    (_ l2))))) ; _ is a variable

