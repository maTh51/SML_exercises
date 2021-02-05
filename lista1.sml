(* List 1 *)

(* 1 *)
fun cube  (x:real) = x * x * x;

(* cube (~3.0); *)


(* 2 *)
fun pow (x, 0) = 1
  | pow (x, 1) = x
  | pow (x,y) = if y>0 then x * pow(x, y-1) else 1 div pow(x, (~y));

(* pow(2,~2); *)


(* 3 *)
fun sumLists([],[]) = []
  | sumLists(x1, []) = x1
  | sumLists([], x2) = x2
  | sumLists(h1::t1, h2::t2) = [(h1 + h2)] @ sumLists(t1,t2);

(* sumLists ([2, 5, 10],[1, 15, 4]); *)


(* 4 *)
fun max [] = ~1
  | max(h::[]) = h
  | max(h::t) = 
    let 
      val aux = max(t) 
    in 
      if h>aux then h else aux 
    end;

(* max([2, 1, 7, 3]); *)


(* 5 *)
fun cumSum [] = []
  | cumSum([h]) = [h]
  | cumSum(h::hs::t) = [h] @ cumSum((h+hs)::t);

(* cumSum([6, 10, 3, 11]); *)


(* 6 *)
fun greet(s) = if size s <= 0 then "Hello nobody" else "Hello " ^ s;

(* greet("Janis");
greet(""); *)


(* 7 *)
fun split s =  
  let
    fun separate_string(c) = c = #" " orelse c = #"," orelse c = #"." orelse c = #"-" 
  in
    String.tokens separate_string s
  end;

(* split "Bom dia,pra-voce"; *)


(* 8 *)
fun allTrue([]) = true (*se não tem nenhum elemento, não tem nenhum elemento falso*)
	| allTrue([h]) = h
	| allTrue(h::t) = if h then h andalso allTrue(t) else false;

(* allTrue [true, true, false, true];
allTrue [true, true, true]; *)


(* 9 *)
datatype dinheiro = Centavos of int | Real of real | Pessoa_Dinheiro of string * real;

fun amount(Centavos c) = c
	| amount(Real r) = Real.floor(100.0*r)
	| amount(Pessoa_Dinheiro (nome, r)) = Real.floor(100.0*r);

(* amount(Real(2.0));
amount(Centavos(2));
amount(Pessoa_Dinheiro("Gene", 2.5)); *)


(* 10 *)
datatype Planeta = Mercurio | Venus | Terra | Marte 
					| Jupiter | Saturno | Urano | Netuno;

fun eval Mercurio = 88.0/12.0
	| eval Venus = 225.0/12.0
	| eval Terra = 365.0/12.0
	| eval Marte = 687.0/12.0
	| eval Jupiter = 4332.0/12.0
	| eval Saturno = 10760.0/12.0
	| eval Urano = 30681.0/12.0
	| eval Netuno = 60190.0/12.0;

fun planetAge(month, planet) = floor( (real month) * eval(planet) );

(* planetAge (24, Jupiter); *)


(* 11 *)
datatype btree = Leaf | Node of (btree * int * btree);

fun sumAll Leaf = 0
  | sumAll (Node (t1, n, t2)) = (sumAll t1) + n + (sumAll t2);

(* sumAll(Node (Node (Leaf, 1, Leaf), 6, Node (Leaf, 12, Leaf))); *)


(* 12 *)
fun multiPairs(a, b) = ListPair.map ( op * ) (a, b);

(* multiPairs([2, 5, 10], [4, 10, 8]); *)


(* 13 *)
fun compose(sum_number, square_number, x, y) = square_number(sum_number(x, y));

fun sum_number(x, y) = x + y;
fun square_number x = x * x;

(* compose(sum_number, square, 4, 5); *)


(* 14 *)
(* a) (int * real) -> real;

b) ((int * real -> real) * int) -> real

c) (int * (real * int -> real)) -> real

d) ((((int * int -> int) * real -> real) * (real * int -> real))) -> real *)


(* 15 *)
(* a) O erro ocorre porque em SML todo "if" precisa de um "then" e um "else". 
   O interpretador até espera que o usuário adicione o "else" na linha de 
   baixo, por isso o erro não aparece instantaneamente.

b) Na operação de multiplicação, os tipos dos valores que serão operados 
   tem que ser o mesmo (no exemplo está se tentando multiplicar um 'int'
   com um 'real').

c) O erro em questão está acusando redundância, pois o caso 'fact 0' já é
   coberto pelo 'fact n' na linha anterior. Para evitar isso, e ainda sim
   envolver o 'fact 0', ele deve vir antes do 'fact n'. Todos os casos base 
   devem vir antes do(s) recursivo(s). *)
   