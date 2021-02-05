(* List 2 *)

(* 1 *)
datatype expr = 
					IConst of int 
					| Plus of expr * expr 
					| Minus of expr * expr
					| Multi of expr * expr
					| Div of expr * expr
					| Max of expr * expr
					| Min of expr * expr
					| Eq of expr * expr
					| Gt of expr * expr;

fun eval (IConst i) = i
	| eval (Plus(e1, e2)) = (eval e1) + (eval e2)
	| eval (Minus(e1, e2)) = (eval e1) - (eval e2)
	| eval (Multi(e1, e2)) = (eval e1) * (eval e2)
	| eval (Div(e1, e2)) = if (eval e2) = 0 then 0 else (eval e1) div (eval e2)
	| eval (Max(e1, e2)) = if (eval e1) > (eval e2) then (eval e1) else (eval e2)
	| eval (Min(e1, e2)) = if (eval e1) < (eval e2) then (eval e1) else (eval e2)
	| eval (Eq(e1, e2)) = if (eval e1) = (eval e2) then 1 else 0
	| eval (Gt(e1, e2)) = if (eval e1) > (eval e2) then 1 else 0;

(* val e1 = IConst 2;
eval e1;

val e2 = IConst 3;

val e3 = Minus(Plus(IConst 3, IConst 4), IConst 8);
eval e3;

val e4 = Multi(IConst 3, IConst 4);
eval e4;

val e5 = Div(IConst 2, IConst 0);
eval e5;

val e6 = Gt(Plus(e1, IConst 2), e2);
eval e6;

val e1 = Min(IConst 3, Plus(IConst 2, IConst 3));
eval e1;

val e2 = Div(Multi(IConst 5, IConst 4), Minus(IConst 4, IConst 4));
eval e2; *)


(* 2 *)

datatype area = 
					RConst of real 
					| AQuadrado of area 
					| ACirculo of area 
					| ARetangulo of area * area;

fun eval (RConst r) = r
  | eval (AQuadrado l) = let val la = (eval l) in la * la end
	| eval (ACirculo r) = let val ra = (eval r) in ra * ra * 3.14 end
	| eval (ARetangulo(l1, l2)) = (eval l1) * (eval l2);

(* 
	eval (RConst 2.0);

	val e = ACirculo(RConst 2.0);
	eval e;

	val e1 = AQuadrado(RConst 4.0);
	eval e1;

	val e2 = ARetangulo(e, e1);
	eval e2; *)


(* 3 *)
datatype perimetro = 
					RConst of real 
					| PQuadrado of perimetro 
					| PCirculo of perimetro 
					| PRetangulo of perimetro * perimetro
					| PTriangulo of perimetro * perimetro * perimetro;

fun eval (RConst r) = r
  | eval (PQuadrado l) = 4.0 * (eval l)
	| eval (PCirculo r) = 2.0 * (eval r) * 3.14
	| eval (PRetangulo(l1, l2)) = 2.0 * ((eval l1) + (eval l2))
	| eval (PTriangulo(l1, l2, l3)) = (eval l1) + (eval l2) + (eval l3);


	(* eval (RConst 2.0);

	val e = PCirculo(RConst 3.0);
	eval e;

	val e1 = PQuadrado(RConst 4.0);
	eval e1;

	val e2 = PRetangulo(e, e1);
	eval e2; *)


(* 4 *)
datatype UnOp = Not;
datatype BinOp = Add | Sub | Mul | Gt | Eq | Or;
datatype Sexpr = IConst of int | Op1 of UnOp * Sexpr | Op2 of BinOp * Sexpr * Sexpr;
 
fun simplify(Op1(Not, Op1(Not, s))) = (simplify s)
	| simplify(Op1(u1, s)) = Op1(u1, (simplify s))

	| simplify(Op2(Add, s1, s2)) = 
		if (simplify s1) = IConst 0 
		then (simplify s2)
		else 
			if (simplify s2) = IConst 0
			then (simplify s1)
			else Op2(Add, (simplify s1), (simplify s2))
	
	| simplify(Op2(Sub, s1, s2)) = 
		if (simplify s2) = IConst 0 
		then (simplify s1)
		else 
			if (simplify s1) = (simplify s2)
			then IConst 0
			else Op2(Add, (simplify s1), (simplify s2))
	
	| simplify(Op2(Mul, s1, s2)) = 
		if (simplify s1) = IConst 1 
		then (simplify s2)
		else 
			if (simplify s2) = IConst 1 
			then (simplify s1) 
			else 
				if (simplify s1) = IConst 0
				then IConst 0
				else 
					if (simplify s2) = IConst 0
					then IConst 0
					else Op2(Mul, (simplify s1), (simplify s2))
	
	| simplify(Op2(Or, s1, s2)) = 
		if (simplify s1) = (simplify s2) 
		then simplify s1
		else Op2(Or, (simplify s1), (simplify s2))

	| simplify(s) = s;

(* val e1 = Op2(Mul, Op2(Add, IConst 1, IConst 0), Op2(Add, IConst 9, IConst 0));
simplify(e1);

val e2 = Op2(Mul, Op2(Add, IConst 1, IConst 0), Op1(Not, Op2(Or, IConst 10, IConst 10)));
simplify(e2);

val e3 = Op2 (Mul, Op2 (Add, IConst 1, IConst 0), Op2 (Add, Op2 (Or, IConst 10, IConst 10), IConst 0));
simplify e3; *)


(* 5 *)

(*x = m / y = n -> z = n^m*)
(* z=1; while ~(y=0) do (z := x*z; y := y-1) *)


(* 6 *)

(*x = 3*)

(* B[~(x=1)]s =
B[~B(A[x]s = A[1])] =
B[~B(s x = N[1])] =
B[~B(3 = 1)] =
B[~ff] = tt *)


(* 7 *)

(* true[y->a0] = true
false[y->a0] = false
(a1 = a2)[y->a0] = (a1[y->a0]) = (a2[y->a0])
(a1 <= a2)[y->a0] = (a1[y->a0]) <= (a2[y->a0])
(~b)[y->a0] = ~(b[y->a0])
(a1 ∧ a2)[y->a0] = (a1[y->a0]) ∧ (a2[y->a0]) *)


(* 8 *)

                                                                                                                                            (* <z := z+1, sz2> -> sz3 | <x := x-y, sz2> -> sx3
                                                                                                                                   __________________________________________________
                                                                           <z := z+1, sz1> -> sz2 | <x := x-y, sz1> -> sx2                 <(z := z+1; x := x-y), sz2> -> sx3
                                                                          _________________________________________________      ____________________________________________________
                    <z := z+1, sz0> -> sz1 | <x := x-y, sz0> -> sx1                 <(z := z+1; x := x-y), sz1> -> sx2        |     <while y ≤ x do (z := z+1; x := x-y), sz2> -> sx3>
                    _______________________________________________       ____________________________________________________________________________________________________________
                           <(z := z+1; x := x-y), sz0> -> sx1         |                               <while y ≤ x do (z := z+1; x := x-y), sz1> -> sx2>
                    ______________________________________________________________________________________________________________________________________________
<z := 0, s> -> sz0         |          <while y ≤ x do (z := z+1; x := x-y), sz0> -> sx3>
__________________________________________________________________________________________________________________________________________________________________
    <z := 0; while y ≤ x do (z := z+1; x := x-y), s> -> sx3


sz0 = s[z->0]

sz1 = s[z->1]
sx1 = s[x->12]

sz2 = s[z->2]
sx2 = s[x->7]

sz3 = s[z->3]
sx3 = s[x->2] *)


(* 9 *)

(* a) Não termina caso o valor inicial de x seja menor que 1.

b) Termina pois, caso x seja menor que 1, o loop não é iniciado. Caso contrário, x eventualmente atingirá um valor menor menor que 1, saindo do loop.

c) Não termina. A avaliação de true sempre é true. *)


(* 10 *)

type Num = int;
type Var = string;

datatype Aexpr = 
	N of Num 
	| V of Var 
	| Plus of Aexpr * Aexpr 
	| Mult of Aexpr * Aexpr 
	| Minus of Aexpr * Aexpr;

datatype Bexpr = 
	True 
	| False 
	| Eq of Aexpr * Aexpr 
	| Leq of Aexpr * Aexpr 
	| Not of Bexpr 
	| And of Bexpr * Bexpr;

datatype Stm =
	Assign of Var * Aexpr
	| Skip
	| Comp of Stm * Stm
	| If of Bexpr * Stm * Stm
	| While of Bexpr * Stm
	| Repeat of Bexpr * Stm;

fun evalN n : Num = n;

exception FreeVar;

fun lookup [] id = raise FreeVar
	| lookup (( k : string , v ) :: l ) id = if id = k then v else lookup l id;

fun evalA ( N n ) _ = evalN n
	| evalA( V x ) s = lookup s x
	| evalA( Plus ( e1 , e2 ) ) s = ( evalA e1 s ) + ( evalA e2 s )
	| evalA( Mult ( e1 , e2 ) ) s = ( evalA e1 s ) * ( evalA e2 s )
	| evalA( Minus ( e1 , e2 ) ) s = ( evalA e1 s ) - ( evalA e2 s );

fun evalB True _ = true
	| evalB False _ = false
	| evalB ( Eq ( a1 , a2 ) ) s = ( evalA a1 s ) = ( evalA a2 s )
	| evalB ( Leq ( a1 , a2 ) ) s = ( evalA a1 s ) <= ( evalA a2 s )
	| evalB ( Not b ) s = not ( evalB b s )
	| evalB ( And ( b1 , b2 ) ) s = ( evalB b1 s ) andalso ( evalB b2 s );

fun evalStm ( stm : Stm ) ( s : ( string * int ) list ) : ( string * int ) list =
  case stm of (Assign (x, a)) => (x , evalA a s ) :: s
  | Skip => s
  | (Comp (stm1, stm2)) => evalStm stm2 ( evalStm stm1 s )
  | (If (b , stm1 , stm2 ) ) => if ( evalB b s ) then evalStm stm1 s else evalStm stm2 s
  | (While (b, stm)) => if (evalB b s) then evalStm (While(b, stm)) (evalStm stm s) else s
  | (Repeat (b, stm)) => if (evalB b s) then s else evalStm (Repeat(b, stm)) (evalStm stm s)
  | _ => raise Match;


(* 11 *)

(* 
a) 1

b) 2 
*)


(* 12 

a)  
Escopo de g = bloco 1 -> função g - linhas de 1 à 13
Escopo de g' = bloco 2 -> let entre as linhas 2 à 13 (dentro da função g)
Escopo de f = bloco 3 -> função f - linha 4
Escopo de h = bloco 4 -> função h - linhas 5 à 10
Escopo de h' = bloco 5 ->let entre as linhas 6 e 10 (função h)

b)  
g -> função
inc -> val
f -> função
y -> val (parametro)
h -> função
z -> val (parametro)
x -> val (parametro)

c)  
g está no escopo geral;
inc está definido no bloco 2 e no bloco 5;
f está definido no bloco 2;
y está definido no bloco 3;
h está definido no bloco 2;
z está definido no bloco 4;
x está definido no bloco 1.

d)  
g 5 seria igual à 6, enquanto se SML possuíse escopo dinâmico 
seria 7. Os valores são diferentes porque com o escopo dinâmico o 
valor de inc seria definido pela função h, mais especificamente 
pelo let, o que não ocorre escopo estático. *)