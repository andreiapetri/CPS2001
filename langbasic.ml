(* ----------------------------- Type definitions ----------------------------*)

(** Alphabet set of characters. *)
type alpha = char

(** 'Not in alphabet' exception. *)
exception Not_in_alphabet of alpha


(* ----------------------------- Supporting code ---------------------------- *)
(** [map f s] 
    Maps every element in [s] using the function [f]. 
*)
let rec map (f: 'a -> 'b) (s: 'a list): 'b list =
  match s with 
  | []   -> []
  | h::t -> (f h) :: (map f t)


(* ------------------------------ Implementation -----------------------------*)

(** Question 1.
    [num_of_c c s] 
    Returns the number of occurrences of [c] in list of characters [s].
*)
let rec num_of_c (c : alpha) (s : alpha list): int =
  match s with
  | [] -> 0
  (* If the head is our target, c, add 1 and recall func. else just recall func. *)
  | h :: t -> if h = c then 1 + num_of_c c t else num_of_c c t

(** Question 2. *)
let rec has_c (c : alpha) (s : alpha list): bool =
  match s with
  | [] -> false
  (* If head matches, return true directly, else, recall func and store in its tail. *)
  | h :: t -> h = c || has_c c t

(** Question 3. *)
let rec flt_c (c : alpha) (s : alpha list): alpha list =
  match s with
  | [] -> []
  (* If head is c, skip it. else, continue calling the func. *)
  | h :: t -> if h = c then flt_c c t else h :: flt_c c t

(** Question 4. *)
let rec invert (s : alpha list): alpha list =
  match s with
  | [] -> []
  | h :: t ->
    let inverted_h = 
      if h = 'a' then 'b' 
      else if h = 'b' then 'a' 
      else raise (Not_in_alphabet h)
    in
    inverted_h :: invert t

(** Question 5. *)
let rec len_acc (s : alpha list) (n : int): int =
  match s with
  | [] -> n
  (* Tail recursive with accumulator built as parameter of the func to simulate imperativity recursively
  this way is safer to compute in case of the function calling itself too many times. *)
  | _ :: t -> len_acc t (n + 1)

(** Question 5 (wrapper). *)
let len (s : alpha list): int = len_acc s 0

(** Question 6. *)
let inv_map (s : alpha list): alpha list =  
  (* I attempted the auxilary function, mentioned in the pdf, so far tests seem fine so hope its working fine )) *)
  let invert_char c = 
    if c = 'a' then 'b' 
    else if c = 'b' then 'a' 
    else raise (Not_in_alphabet c)
  in
  map invert_char s

(** Question 7. *)
let concat (s : alpha list) (t : alpha list): alpha list =
  (* To be tail-recursive, I used an accumulator here once again. 
     Hence, reversed s into it, and then preappended its elements to t. *)
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | h :: tl -> aux tl (h :: acc)
  in
  let rev_s = aux s [] in
  aux rev_s t
;;

