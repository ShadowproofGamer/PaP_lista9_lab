type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;
let rec lfrom k = LCons (k, function () -> lfrom (k+1));;
let rec ltake = function
| (0, _) -> []
| (_, LNil) -> []
| (n, LCons(x,xf)) -> x::ltake(n-1, xf());;


let rec lrepeatExponential (n, llx)=
    let rec lrepeatExponential_rec(i, x, init, lazyRest) =
        if n > i then LCons((x*init), function () -> (lrepeatExponential_rec ((i+1), (x*init), init, lazyRest)))
        else LCons((x*init), lazyRest) in

    match llx with
    | LNil -> LNil
    | LCons(x, xf) -> lrepeatExponential_rec(1, 1, x, function () -> (lrepeatExponential((n+1), xf())));;



ltake(15,(lrepeatExponential(1,(lfrom 1))));;

