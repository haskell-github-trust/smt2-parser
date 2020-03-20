; Axiom for list append: version 1
; List is a parametric datatype
; with constructors "nil" and "cons"
;
(forall ((l1 (List Int)) (l2 (List Int)))
  (= (append l1 l2)
  (match l1 (
    (nil l2)
    ((cons h t) (cons h (append t l2)))))))

