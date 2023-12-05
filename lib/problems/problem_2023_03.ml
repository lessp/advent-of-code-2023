open Base

let year = 2023
let day = 3

module Matrix = struct
  type t = char list list

  let get matrix ~x ~y = List.nth matrix y |> Option.bind ~f:(fun row -> List.nth row x)

  let adjacent matrix ~x ~y ~direction =
    match direction with
    | `Left -> get matrix ~x:(x - 1) ~y
    | `Right -> get matrix ~x:(x + 1) ~y
    | `Up -> get matrix ~x ~y:(y - 1)
    | `Down -> get matrix ~x ~y:(y + 1)
    | `Up_left -> get matrix ~x:(x - 1) ~y:(y - 1)
    | `Up_right -> get matrix ~x:(x + 1) ~y:(y - 1)
    | `Down_left -> get matrix ~x:(x - 1) ~y:(y + 1)
    | `Down_right -> get matrix ~x:(x + 1) ~y:(y + 1)
  ;;

  let adjacents matrix ~x ~y =
    let directions =
      [ `Left; `Right; `Up; `Down; `Up_left; `Up_right; `Down_left; `Down_right ]
    in

    directions
    |> List.filter_map ~f:(fun direction ->
      adjacent matrix ~x ~y ~direction |> Option.map ~f:(fun c -> c, direction))
  ;;

  let adjacents_matching matrix ~x ~y ~f =
    adjacents matrix ~x ~y |> List.filter ~f:(fun (c, _) -> f c)
  ;;

  let has_adjacent matrix ~x ~y ~f =
    not (List.is_empty (adjacents_matching matrix ~x ~y ~f))
  ;;

  (* Parsing *)
  let parse input = input |> String.split_lines |> List.map ~f:String.to_list
end

module Part_1 = struct
  (*       .--- not adjacent     *)
  (*       |                     *)
  (* 467..114..                  *)
  (* ...*......                  *)
  (* ..35..633.                  *)
  (* ......#...                  *)
  (* 617*......                  *)
  (* .....+.58. --- not adjacent *)
  (* ..592.....                  *)
  (* ......755.                  *)
  (* ...$.*....                  *)
  (* .664.598..                  *)
  (*                             *)
  (* Sum: 4361                   *)

  (* We can parse left to right.
     Whenever we come across a number, we check (in every direction) if it is adjacent to a symbol.
     If it is, we add it to the list of part numbers. If it is not, we continue. *)

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  ;;

  let is_symbol = function
    | '.' -> false
    | x when is_digit x -> false
    | _ -> true
  ;;

  let solve (matrix : Matrix.t) =
    let add_if_adjacent numbers ~number ~keep =
      if keep && not (String.is_empty number) then
        Int.of_string number :: numbers
      else
        numbers
    in

    let rec loop_row row ~x ~y ~numbers ~number ~keep =
      match row with
      (* attempt to add if we reach the end of a row *)
      | [] -> add_if_adjacent numbers ~number ~keep
      (* build up a number if we come across a digit *)
      | c :: rest when is_digit c ->
        let number = number ^ Char.to_string c in
        let keep = keep || Matrix.has_adjacent matrix ~x ~y ~f:is_symbol in

        loop_row rest ~x:(x + 1) ~y ~numbers ~number ~keep
      (* if we come across something different from a digit and have a number in progress,
         attempt to add it *)
      | _c :: rest when String.length number > 0 ->
        let new_numbers = add_if_adjacent numbers ~number ~keep in

        loop_row rest ~x:(x + 1) ~y ~numbers:new_numbers ~number:"" ~keep:false
      (* if we come across a non-digit and don't have a number in progress, simply continue *)
      | _c :: rest -> loop_row rest ~x:(x + 1) ~y ~numbers ~number:"" ~keep:false
    in

    let rec loop_matrix matrix ~y ~numbers =
      match matrix with
      | [] -> numbers
      | row :: rest ->
        loop_matrix
          rest
          ~y:(y + 1)
          ~numbers:(loop_row row ~x:0 ~y ~numbers ~number:"" ~keep:false)
    in

    loop_matrix matrix ~y:0 ~numbers:[] |> List.fold ~init:0 ~f:( + ) |> Int.to_string
  ;;

  let run (input : string) : (string, string) Result.t =
    Ok (input |> Matrix.parse |> solve)
  ;;
end

module Part_2 = struct
  open Base

  let run (input : string) : (string, string) Result.t = Ok input
end
