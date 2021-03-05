; Instance after running `cfg_red`.

(set-logic HORN)

; Datatypes

; Functions

; Side-clauses


(declare-fun
  |fib_without_checking_1060$unknown:16|
  ( Int Int Int ) Bool
)
(declare-fun
  |fib_without_checking_1060$unknown:15|
  ( Int Int Int ) Bool
)

; Original clauses' names (0) {
; }

; Clause #0
;   from: #14
;   3 inactive variable(s)
;   unroll: false
;   terms_changed: false
;   preds_changed: false
;   created by `parsing`
(assert 
  (forall
    ( (|$alpha-10:s_fib_n_1049| Int) (|$alpha-11:n_1031| Int) (|$alpha-9:set_flag_fib_1052| Int) )
    (=>
      (and
        (>= |$alpha-11:n_1031| 2)
        (|fib_without_checking_1060$unknown:15| |$alpha-11:n_1031| |$alpha-10:s_fib_n_1049| |$alpha-9:set_flag_fib_1052|)
      )
      (|fib_without_checking_1060$unknown:15| (+ (- 1) |$alpha-11:n_1031|) |$alpha-11:n_1031| 1)
    )
  )
)



; Clause #1
;   from: #10
;   6 inactive variable(s)
;   unroll: false
;   terms_changed: false
;   preds_changed: false
;   created by `parsing`
(assert 
  (forall
    ( (|$V-reftype:25| Int) (|$alpha-10:s_fib_n_1049| Int) (|$alpha-9:set_flag_fib_1052| Int) (|$knormal:28| Int) (hoice_fresh_var@11 Int) (hoice_fresh_var@12 Int) )
    (=>
      (and
        (>= |$V-reftype:25| 2) (>= |$knormal:28| 1)
        (|fib_without_checking_1060$unknown:16| |$knormal:28| (+ 1 |$knormal:28|) 1) (|fib_without_checking_1060$unknown:16| (+ (- 1) |$V-reftype:25|) |$V-reftype:25| 1) (|fib_without_checking_1060$unknown:15| |$V-reftype:25| hoice_fresh_var@11 hoice_fresh_var@12) (|fib_without_checking_1060$unknown:15| (+ 1 |$knormal:28|) |$alpha-10:s_fib_n_1049| |$alpha-9:set_flag_fib_1052|)
      )
      (|fib_without_checking_1060$unknown:15| (+ |$V-reftype:25| (- 2)) |$V-reftype:25| 1)
    )
  )
)



; Clause #2
;   from: #15
;   1 inactive variable(s)
;   unroll: false
;   terms_changed: false
;   preds_changed: false
;   created by `parsing`
(assert 
  (forall
    ( (|$alpha-14:r| Int) )
    (=>
      (and
        true
        true
      )
      (|fib_without_checking_1060$unknown:15| |$alpha-14:r| 0 0)
    )
  )
)



; Clause #3
;   from: #13
;   4 inactive variable(s)
;   unroll: false
;   terms_changed: false
;   preds_changed: false
;   created by `parsing`
(assert 
  (forall
    ( (|$alpha-10:s_fib_n_1049| Int) (|$alpha-11:n_1031| Int) (|$alpha-9:set_flag_fib_1052| Int) (|$knormal:24| Int) )
    (=>
      (and
        (>= (* (- 1) |$alpha-11:n_1031|) (- 1)) (not (= |$knormal:24| 0))
        (|fib_without_checking_1060$unknown:15| |$alpha-11:n_1031| |$alpha-10:s_fib_n_1049| |$alpha-9:set_flag_fib_1052|)
      )
      (|fib_without_checking_1060$unknown:16| |$alpha-11:n_1031| |$alpha-10:s_fib_n_1049| |$alpha-9:set_flag_fib_1052|)
    )
  )
)



; Clause #4
;   from: #9
;   10 inactive variable(s)
;   unroll: false
;   terms_changed: false
;   preds_changed: false
;   created by `parsing`
(assert 
  (forall
    ( (|$alpha-10:s_fib_n_1049| Int) (|$alpha-9:set_flag_fib_1052| Int) (|$knormal:28| Int) (hoice_fresh_var@10 Int) (hoice_fresh_var@11 Int) (hoice_fresh_var@12 Int) (hoice_fresh_var@13 Int) (hoice_fresh_var@14 Int) (hoice_fresh_var@15 Int) (hoice_fresh_var@16 Int) )
    (let
      ( (v_17 (+ 1 |$knormal:28|)) )
      (=>
        (and
          (>= hoice_fresh_var@16 2) (>= |$knormal:28| 1)
          (|fib_without_checking_1060$unknown:16| (+ (- 1) |$knormal:28|) v_17 1) (|fib_without_checking_1060$unknown:16| |$knormal:28| v_17 1) (|fib_without_checking_1060$unknown:16| (+ (- 1) hoice_fresh_var@16) hoice_fresh_var@16 1) (|fib_without_checking_1060$unknown:15| hoice_fresh_var@16 hoice_fresh_var@10 hoice_fresh_var@11) (|fib_without_checking_1060$unknown:15| v_17 hoice_fresh_var@12 hoice_fresh_var@15) (|fib_without_checking_1060$unknown:15| v_17 hoice_fresh_var@14 hoice_fresh_var@13) (|fib_without_checking_1060$unknown:15| v_17 |$alpha-10:s_fib_n_1049| |$alpha-9:set_flag_fib_1052|)
        )
        (|fib_without_checking_1060$unknown:16| v_17 |$alpha-10:s_fib_n_1049| |$alpha-9:set_flag_fib_1052|)
      )
    )
  )
)



(check-sat)
