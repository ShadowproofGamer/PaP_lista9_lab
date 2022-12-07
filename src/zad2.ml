type 'a slowo = int * int;;
type 'a slownik = (int * int) list;;

let remover i s =
  let rec remover_rec remaining res =
    match remaining with
      (x, y)::t when x=i && y>1 -> remover_rec t ((x, (y-1))::res)
     |(x, y)::t when x=i && y=1 -> remover_rec t res
     |(x, y)::t when x!=i -> remover_rec t ((x, y)::res)
     | _ -> res
  in List.rev (remover_rec s []);;

let sl = [(1,1);(2,2);(3,3);(4,4)];;
let xl = remover 2 sl;;
remover 2 xl;;
remover 4 xl;;
