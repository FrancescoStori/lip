open Types

(* Use this grammar structure as a blueprint for the exercises. *)
let todo : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [
        S --> "0S0";
        S --> "1S1";
        S --> "";
      ];
    start = S;
  }


(* #### Exercise 1, easy (zero_n_one_n) *)
let zero_n_one_n : grammar = 
  {
    symbols = [ S ];
    terminals = [ '0'; '1'];
    productions =
      [
        S --> "0S1";
        S --> "";
      ];
    start = S;
  }


(* #### Exercise 2, easy (palindromes) *)
let palindromes : grammar = 
  {
    symbols = [ S ];
    terminals = [ '0'; '1'];
    productions =
      [
        S --> "0S0";
        S --> "1S1";
        S --> "0";
        S --> "1";
        S --> "";
      ];
    start = S;
  }


(* #### Exercise 3, medium (balanced_parentheses)*)
let balanced_parentheses : grammar = 
  {
    symbols = [ S ; A ];
    terminals = [ '(';')';'[';']';'{';'}'];
    productions =
      [
        S --> "(A)S"; (*0*)
        S --> "[A]S"; (*1*)
        S --> "{A}S"; (*2*)
        S --> "";     (*3*)
        A --> "(S)A"; (*4*)
        A --> "[S]A"; (*5*)
        A --> "{S}A"; (*6*)
        A --> ""      (*7*)
      ];
    start = S;
  }



(* #### Exercise 4, hard (same_amount)

   Hint 1: you can use 'a' and 'b' for terminals.
   Hint 2: think of the language of words where the number of 0s is one greater
   than the number of 1s and viceversa, then combine them.
*)
(*let same_amount : grammar = 
  {
    symbols = [S; A; B];
    terminals = ['0'; '1'];
    productions = [
      S --> "";
      S --> "0AS";
      S--> "1BS";
      A --> "1S";
      B --> "0S";
    ];
    start = S;
  }
*)

let same_amount : grammar = 
  {
    symbols = [S; A; B];
    terminals = ['0'; '1'];
    productions = [
      S --> "";
      S --> "0A";
      S--> "1B";
      A --> "1";
      B --> "0";
      A --> "AS";
      A --> "SA";
      B --> "BS";
      B --> "SB"
    ];
    start = S;
  }
