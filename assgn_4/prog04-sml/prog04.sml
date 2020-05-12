(* 
 * Name: Fernando Flores Hernandez
 * Time spent on assignment: 6hrs
 * Collaborators: 
 *)

(* NOTE: Semicolons are not required by SML to separate definitions,
 * but MoscowML will give earlier/better error messages with them.
 *)

(* **************************************** *)
(* **************************************** *)

exception Unimplemented of string

fun fst (x, _) = x

fun snd (_, y) = y

fun foldl (f : 'a * 'b -> 'b) (b: 'b) (l: 'a list): 'b =
  case l of
     [] => b
   | h::t => foldl f (f (h, b)) t

fun foldr (f : 'a * 'b -> 'b) (b: 'b) (l: 'a list): 'b =
  case l of
     [] => b
   | h::t => f (h, foldr f b t)

;

(* **************************************** *)
(* **************************************** *)


(* Part A (unzip/zip) *)

(* A.a *)
(* DEFINE unzip HERE *)
fun unzip (xys : ('a * 'b) list) : 'a list * 'b list =
case xys of
   [] => ([],[])
   | h::t => ((fst h) :: (fst (unzip t)) , (snd h) :: (snd (unzip t)))
;

(* A.b *)
(* DEFINE zip HERE *)
fun helperfoldr f z ((x::xs), (y::ys)) = f (x, y, helperfoldr f z (xs, ys))
  | helperfoldr f z (_,  _) = z;

fun zip (xs, ys) = helperfoldr (fn (x, y, z) => (x, y)::z) [] (xs, ys);
;

(* **************************************** *)
(* **************************************** *)


(* Part B *)

(* DEFINE compound HERE *)

fun compound 0 f x = x
  | compound n f x = compound (n - 1) f (f x);  


(* **************************************** *)
(* **************************************** *)


(* Part C *)

(* DEFINE exp HERE *)
fun exp 0 0 = 1
  | exp b 0 = 1
  | exp b e = (compound (e - 1) (fn x => b * x ) b); 
;

(* **************************************** *)
(* **************************************** *)


(* Part D *)

(* DEFINE existsUnique HERE *)
fun counting _ [] = 0
  | counting f (x::xs) = 
    if (f x)=true 
      then 1 + (counting f xs)
    else (counting f xs)

fun helper f alist = 
  if (counting f alist) > 1 then false
  else 
    if(counting f alist) < 1 then false
    else true

fun existsUnique f alist = (helper f alist);



(* **************************************** *)
(* **************************************** *)


(* Part E *)

(* DEFINE allAlt HERE *)
fun allAlt f [] = true 
  (* If theres only one elemet *)
  | allAlt f [x] = 
    if (f x) = true then 
      true
    else 
      false
  (* Alternate between even and odd then recurse *)
  | allAlt f (x::y::xs) = 
    if (f x) = true andalso (f y) = false then 
      (allAlt f xs)
    else
      false
;

(* **************************************** *)
(* **************************************** *)


(* Part F *)
(* DEFINE separate HERE *)

fun separate (k: int, x: 'a, l: 'a list) : 'a list =
 let
   fun kinsert [] _ = []
     | kinsert ls 0 = x::(kinsert ls k)
     | kinsert (l::ls) i = l::(kinsert ls (i-1))
 in
   List.rev (kinsert (List.rev l) k)
 end


fun onesSeparate i =
   implode (separate (1, #",", explode (Int.toString i)))
fun tensSeparate i =
   implode (separate (2, #",", explode (Int.toString i)))
fun thousandsSeparate i =
   implode (separate (3, #",", explode (Int.toString i)))

;

(* **************************************** *)
(* **************************************** *)

type name = string
fun nameCompare (n1: name, n2: name) : order = String.compare (n1, n2)
fun nameEqual (n1: name, n2: name) : bool =
   case nameCompare (n1, n2) of EQUAL => true | _ => false
exception NotFound of name

;

(* list representation for environments *)
type 'a lenv = (name * 'a) list
val lenvEmpty = []
fun lenvFind (name, rho) =
   case rho of
      [] => raise NotFound name
    | (n, d)::tail =>
         if nameEqual (name, n) then d else lenvFind (name, tail)
fun lenvBind (name, data, rho) = (name, data) :: rho

;

(* **************************************** *)
(* **************************************** *)


(* Part G (binary search tree representation for environments) *)

datatype 'a btree = Leaf | Node of 'a btree * 'a * 'a btree

fun btreeInsert cmp =
   let
      fun ins (x, btree) =
         case btree of
            Leaf => Node (Leaf, x, Leaf)
          | Node (lt, y, rt) =>
               (case cmp (x, y) of
                   LESS => Node (ins (x, lt), y, rt)
                 | EQUAL => Node (lt, x, rt)
                 | GREATER => Node (lt, y, ins (x, rt)))
   in
      ins
   end
val _ : ('a * 'a -> order) -> ('a * 'a btree) -> 'a btree = btreeInsert

fun btreeLookup cmp =
   let
      fun lkup (x, btree) =
         case btree of
            Leaf => NONE
          | Node (lt, y, rt) =>
               (case cmp (x, y) of
                   LESS => lkup (x, lt)
                 | EQUAL => SOME y
                 | GREATER => lkup (x, rt))
   in
      lkup
   end
val _ : ('a * 'a -> order) -> ('a * 'a btree) -> 'a option = btreeLookup
val _ : ('a * 'b -> order) -> ('a * 'b btree) -> 'b option = btreeLookup

;

type 'a tenv = (name * 'a) btree

;

(* G.a *)
(* DEFINE tenvEmpty HERE *)
val tenvEmpty = Leaf
;

(* G.b *)
(* DEFINE tenvFind HERE *)

fun tenvFind(name, rho) = 
   let 
    fun helperFun compare name Leaf = raise NotFound name 
      | helperFun compare name (Node (lt, (key, value), rt)) = 
       case compare(name, key) of
           LESS => helperFun compare name lt
         | GREATER => helperFun compare name rt
         | EQUAL => value
   in
      (helperFun nameCompare name rho)
   end;

(* G.c *)
(* DEFINE tenvBind HERE *)

fun tenvBind(name, data, rho) = 
   let
      fun inserting compare name data Leaf = Node(Leaf, (name, data), Leaf)
      | inserting compare name data (Node (lt, (key, value), rt)) = 
      case compare(name, key) of
         LESS => Node((inserting compare name data lt), (key, value), rt)
         | GREATER => Node(lt, (key, value), (inserting compare name data rt))
         | EQUAL => Node(lt, (name, data), rt)
   in
      (inserting nameCompare name data rho)
   end;


(* **************************************** *)
(* **************************************** *)


(* Part H (function representation for environments) *)

type 'a fenv = name -> 'a

;

(* H.a *)
(* DEFINE fenvEmpty HERE *)
val fenvEmpty = fn name => raise NotFound name
;

(* H.b *)
(* DEFINE fenvFind HERE *)
fun fenvFind(name: name, rho: 'a fenv) = (rho name)
;
(* H.c *)
(* DEFINE fenvBind HERE *)
fun fenvBind(name, data, rho) = fn key => if key = name then data else rho key 
;

(* **************************************** *)
(* **************************************** *)


(* Part I (append lists) *)

datatype 'a alistNN = Sing of 'a | Append of 'a alistNN * 'a alistNN
datatype 'a alist = Nil | NonNil of 'a alistNN

;

(* I.a *)
(* DEFINE alistAppend HERE *)
(* I could not do this without Nil *)
fun alistAppend (xs: 'a alist, ys: 'a alist): 'a alist =
  let
    val paired = (xs ,ys);
  in
    case paired of (Nil,ys) => ys
      |(xs,Nil) => xs
      |(NonNil xs, NonNil ys) => NonNil (Append(xs,ys))
  end

;

(* I.b *)
(* DEFINE alistCons HERE *)
fun alistCons (x: 'a, xs: 'a alist): 'a alist =
   case (x,xs) of 
      (x,Nil) => NonNil (Sing x)
      |(_, NonNil xs) => NonNil (Append (Sing x ,xs)) ;


fun alistUncons (xs: 'a alist) : ('a * 'a alist) option =
   case xs of
      Nil => NONE
    | NonNil xs =>
         let
            fun helperFun (xs: 'a alistNN) : 'a * 'a alist =
               case xs of
                  Sing x => (x, Nil)
                | Append (ys, zs) =>
                     let
                        val (w, ws) = helperFun ys
                     in
                        (w, alistAppend (ws, NonNil zs))
                     end
         in
            SOME (helperFun xs)
         end

;

(* I.c *)
(* DEFINE alistSnoc HERE *)
fun alistSnoc (xs: 'a alist, x: 'a): 'a alist =
    case (xs, x) of 
      (Nil, x )=> NonNil ( Sing x ) 
      | (NonNil xs, x) => NonNil (Append(xs, Sing x))
;

(* I.d *)
(* DEFINE alistUnsnoc HERE *)
(* same as Uncons just with params flipped *)


fun alistUnsnoc (Nil) = NONE
   | alistUnsnoc (NonNil xs) = 
   let
        fun helperFun (xs: 'a alistNN) : 'a alist * 'a =
          case xs of
            Sing x => (Nil, x)
              | Append (zs ,ys) =>
                let
                  val (ws, w) = helperFun ys
                in
                  (alistAppend (NonNil zs, ws), w)
                end
      in
        SOME (helperFun xs)
      end;

(* I.e *)
(* DEFINE alistMap HERE *)
fun alistMap (f: 'a -> 'b) (xs: 'a alist): 'b alist =
   case xs of 
      Nil => Nil
   	| NonNil xs => 
      let 
        fun helperFun (xs : 'a alistNN) : 'b alistNN =
          case xs 
            of Sing element => Sing (f element)
            | Append (f1, f2) =>
              Append (helperFun f1, helperFun f2)
      in
        NonNil (helperFun xs)
      end
;

(* I.f *)
(* DEFINE alistFilter HERE *)
fun alistFilter (f: 'a -> bool) (xs: 'a alist): 'a alist =
	case xs of Nil => Nil
		| NonNil xs =>
			let
				fun helperFun (xs: 'a alistNN) : ('a alistNN) option =
					case xs of Sing num =>
            if (f num) then SOME (Sing num) else NONE
						| Append (f1, f2) =>	
              case ( helperFun f1, helperFun f2)
								of (NONE, NONE) => NONE
								| (SOME (y), NONE) => SOME(y)
								| (NONE, SOME (z)) => SOME(z)
								| (SOME (y), SOME (z)) => SOME( Append (y, z) )
			in
				case ( helperFun xs) of NONE => Nil
					| SOME(xs) => NonNil xs
			end
;


fun alistFoldr (f: 'a * 'b -> 'b) (b: 'b) (xs: 'a alist) : 'b =
   case xs of
      Nil => b
    | NonNil xs =>
         let
            fun foldrNN (b: 'b) (xs: 'a alistNN): 'b =
               case xs of
                  Sing x => f (x, b)
                | Append (ys, zs) => foldrNN (foldrNN b zs) ys
         in
            foldrNN b xs
         end

;

(* I.g *)
(* DEFINE alistFoldl HERE *)
fun alistFoldl (f: 'a * 'b -> 'b) (b: 'b) (xs: 'a alist) : 'b =
  case xs of
     Nil => b
   | NonNil xs =>
      let
        fun foldl (b: 'b) (xs: 'a alistNN): 'b =
          case xs of
            Sing x => f (x, b)
            | Append (ys, zs) => foldl (foldl b ys) zs
      in
        foldl b xs
      end


;

(* I.h *)
(* DEFINE alistToList HERE *)
fun alistToList (xs: 'a alist) : 'a list =
   raise Unimplemented "alistToList"

;

(* **************************************** *)
(* **************************************** *)


(* Part J (propositional-logic formulas) *)

datatype fmla =
   F_Var of string
 | F_Not of fmla
 | F_And of fmla * fmla
 | F_Or of fmla * fmla

type 'a env = 'a lenv
val envEmpty = lenvEmpty
val envFind = lenvFind
val envBind = lenvBind

;

(* J.a *)
(* DEFINE fmlaSize HERE *)
fun fmlaSize (F_Var _) = 1
  | fmlaSize (F_Not _) = 1 + fmlaSize _
  | fmlaSize (F_And (f1, f2)) = 1 + fmlaSize f1 + fmlaSize f2
  | fmlaSize (F_Or (f1, f2)) = 1 + fmlaSize f1 + fmlaSize f2
;

(* J.b *)
(* DEFINE fmlaVarsOf HERE *)

;

(* J.c *)
(* DEFINE fmlaEval HERE *)

;

(* J.d !bonus!*)
(* DEFINE fmlaTautology HERE *)

;
