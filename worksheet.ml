(** Question 1.
    [is_even x] returns true if [x] is even, otherwise false.
*)
let is_even (x : int): bool =
 if(x mod 2 = 0)then true else false
;;

(** Question 2.
  [remove_int l n] removes every occurrence of [n] from [l].
*)
let rec remove_int (l: int list) (n: int) =
  match l with
  |[] -> []
  |x::xs ->
    if x = n
     then remove_int xs n
    else x :: remove_int xs n
;;

(** Question 3.
  [sum l] returns the summation of the elements of [l].
*)
let sum (l : int list): int =
  let rec aux stack list =
  match list with
  |[] -> stack
  |x::xs -> aux (x+stack) xs
 in aux 0 l
;;
  
(** Question 4.
  [rev l] reverses the elements of [l].
*)
let rev (l : 'a list) : 'a list =
  let rec aux stack list =
    match list with
    |[] -> stack
    |x::xs ->  aux (x :: stack) xs
   in aux [] l
;;

(** Question 5.
  [even_sized_palindrome l] returns the even-sized palindrome of [l].

  Length of result is 2 * [l].
*)
let even_sized_palindrome (l: 'a list): 'a list  =
  match l with
   l -> l @ rev l
;;
(** Question 6.
  [is_palindrome l] returns true if [l] is a palindrome, otherwise false.
*)
let is_palindrome (l: 'a list): bool =
  if(l = rev l) then true else false
;;

  




