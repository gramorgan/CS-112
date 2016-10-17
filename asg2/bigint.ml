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

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

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
         Printf.printf "%i %i\n" car1 car2;
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
        | 1 -> Bigint(neg1, sub' value1 value2 0)
        | -1 -> Bigint((if neg1 = Pos then Neg else Pos),
                          sub' value2 value1 0)
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

    let mul = add

    let div = add

    let rem = add

    let pow = add

end

