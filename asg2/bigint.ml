(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    (* Tail recursion ftw!*)
    let rec insert_newlines str sum charcount = match str with
      | [] -> reverse sum
      | car::cdr -> if charcount = 69 then
         insert_newlines cdr ("\\\n"::car::sum) 1
         else insert_newlines cdr (car::sum) (charcount + 1)
    
    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat "" (insert_newlines (
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))) [] 0)

    let rec rem_leading_zeros list1 = match (reverse list1) with
       | [] -> [0]
       | car1::cdr1 -> if car1 = 0 then rem_leading_zeros (reverse cdr1)
          else list1

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec sub' list1 list2 carry = match (list1, list2, carry) with
       | list1, [], 0 -> list1
       | [], list2, 0 -> list2
       | list1, [], carry -> sub' list1 [carry] 0
       | [], list2, carry -> sub' [carry] list2 0
       | car1::cdr1, car2::cdr2, carry ->
         let diff = car1 - carry - car2
         in if diff < 0 then
         diff + 10 :: sub' cdr1 cdr2 1 else
         diff :: sub' cdr1 cdr2 0

    let rec cmp' list1 list2 = match (list1, list2) with
       | [], [] -> 0
       | list1, [] -> 1
       | [], list2 -> -1
       | car1::cdr1, car2::cdr2 ->
         if car1 > car2 then 1
         else if car1 < car2 then -1
         else cmp' cdr1 cdr2


    let cmp (Bigint (neg1, value1)) (Bigint(neg2, value2)) =
       if neg1 = neg2 then
       let str1 = string_of_bigint (Bigint(neg1, value1)) in
       let str2 = string_of_bigint (Bigint(neg2, value2)) in
       if (strlen str1) > (strlen str2) then 1
       else if (strlen str1) < (strlen str2) then -1
       else cmp' (reverse value1) (reverse value2)
       else if neg1 = Pos then 1 else -1
       

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then let cmpret = 
                  (cmp (Bigint(neg1, value1)) (Bigint(neg2, value2)))in
        match cmpret with
        | 1 -> Bigint(neg1, rem_leading_zeros (sub' value1 value2 0))
        | -1 -> Bigint((if neg1 = Pos then Neg else Pos),
                          rem_leading_zeros (sub' value2 value1 0))
        | _ -> Bigint(Pos, [0])
        else if neg1 = Pos 
        then Bigint(Pos, (add' value1 value2 0))
        else Bigint(Neg, (add' value1 value2 0))

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else if neg1 = Pos
        then sub (Bigint(neg1, value1)) (Bigint(Pos, value2))
        else sub (Bigint(neg2, value2)) (Bigint(Pos, value1))


    let rec print_list list = match list with
       | [] -> ()
       | car::cdr -> printf "%d\n" car;
         print_list cdr

    let double number = add' number number 0

    let rec mul' (multiplier, powerof2, multiplicand') =
       if (cmp (Bigint(Pos, powerof2)) (Bigint(Pos, multiplier))) > 0
       then multiplier, [0]
       else let remainder, product =
             mul' (multiplier, double powerof2, double multiplicand')
          in 
          if (cmp (Bigint(Pos, remainder)) (Bigint(Pos, powerof2))) < 0
              then remainder, product
              else rem_leading_zeros (sub' remainder powerof2 0), 
                                     (add' product multiplicand' 0)

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let _, product = mul' (value1, [1], value2) in
        if neg1 = neg2
            then Bigint (Pos, product)
            else Bigint (Neg, product)
    
    let rec pow' neg1 value1 value2 neg2 product = match value2 with
       | [] -> Bigint(neg2, product)
       | [0] -> Bigint(neg1, value1)
       | [1] -> Bigint(neg2, product)
       | value2 ->
         let Bigint(nneg1, nvalue2) =
            sub (Bigint(Pos, value2)) (Bigint(Pos, [1])) in
         let Bigint(nneg2, nproduct) =
            mul (Bigint(neg2, product)) (Bigint(neg1, value1)) in
         pow' neg1 value1 nvalue2 nneg2 nproduct

     
     let rec divrem' (dividend, powerof2, divisor') =
         if (cmp (Bigint(Pos, divisor')) (Bigint(Pos, dividend))) > 0
         then [0], dividend
         else let quotient, remainder =
            divrem' (dividend, double powerof2, double divisor') in  
            if (cmp (Bigint(Pos, remainder)) (Bigint(Pos, divisor')))< 0
               then quotient, remainder
               else add' quotient powerof2 0, 
                    rem_leading_zeros (sub' remainder divisor' 0)

     let divrem (dividend, divisor') = divrem' (dividend, [1], divisor')

     let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
         let quotient, _ = divrem (value1, value2) in
         if neg1 = neg2 then
            Bigint((if neg1 = Pos then Pos else Neg), quotient)
         else Bigint(Neg, quotient)

     let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
         let _, rem = divrem (value1, value2) in
         if neg1 = Neg then
            Bigint(Neg, rem)
         else Bigint(Pos, rem)

     let even (Bigint(neg1, number)) = let _, rem = divrem(number, [2]) 
         in (cmp (Bigint(Pos, rem)) zero) = 0

     let rec pow' (base: bigint) (expt: bigint) (result: bigint) = 
         match expt with
            | Bigint(_, [0])      -> result
            | expt when even expt -> 
                  pow' (mul (base) (base)) 
                       (div (expt) (Bigint(Pos, [2]))) (result)
            | expt                -> 
                  pow' (base) (sub expt (Bigint(Pos, [1]))) 
                       (mul (base) (result))

     let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg2 = Pos
        then pow' (Bigint(neg1, value1)) (Bigint(neg2, value2)) 
                  (Bigint(Pos, [1]))
        else zero

end

