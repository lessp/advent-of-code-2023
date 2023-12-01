let year = 2023
let day = 1

module Part_1 = struct
  module Trebuchet = struct
    open Base

    let separate_calibrations input = input |> String.split ~on:'\n'

    let find_first_digit chars = chars |> List.find ~f:Char.is_digit
    let find_last_digit chars = chars |> List.rev |> find_first_digit

    let get_calibration_value chars =
      let first_digit = find_first_digit chars in
      let last_digit = find_last_digit chars in

      match first_digit, last_digit with
      | Some first_digit, Some last_digit ->
        [ first_digit; last_digit ] |> String.of_char_list |> Int.of_string
      | _ -> 0
    ;;

    let get_calibration_values lines =
      lines |> List.map ~f:String.to_list |> List.map ~f:get_calibration_value
    ;;

    let solve input =
      input
      |> separate_calibrations
      |> get_calibration_values
      |> List.fold ~init:0 ~f:( + )
    ;;
  end

  let run (input : string) : (string, string) result =
    Ok (input |> Trebuchet.solve |> Int.to_string)
  ;;
end

module Part_2 = struct
  module Trebuchet = struct
    open Base

    let separate_calibrations input = input |> String.split ~on:'\n'

    let rec normalize input =
      let rec loop input normalized =
        match input with
        | [] -> normalized |> List.rev
        | hd :: tail as chars when Char.is_alpha hd ->
          (match scan_number chars with
           | Some (normalized_number, new_tail) ->
             loop new_tail (normalized_number :: normalized)
           | None -> loop tail (hd :: normalized))
        | hd :: tail -> loop tail (hd :: normalized)
      in
      loop input []

    and scan_number chars =
      match chars with
      (* quick n' dirty way to handle e.g. "oneight" *)
      | 'o' :: 'n' :: 'e' :: _ -> Some ('1', List.drop chars 2)
      | 't' :: 'w' :: 'o' :: _ -> Some ('2', List.drop chars 2)
      | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> Some ('3', List.drop chars 4)
      | 'f' :: 'o' :: 'u' :: 'r' :: _ -> Some ('4', List.drop chars 3)
      | 'f' :: 'i' :: 'v' :: 'e' :: _ -> Some ('5', List.drop chars 3)
      | 's' :: 'i' :: 'x' :: _ -> Some ('6', List.drop chars 2)
      | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> Some ('7', List.drop chars 4)
      | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> Some ('8', List.drop chars 4)
      | 'n' :: 'i' :: 'n' :: 'e' :: _ -> Some ('9', List.drop chars 3)
      | _ -> None
    ;;

    let find_first_digit chars = chars |> List.find ~f:Char.is_digit
    let find_last_digit chars = chars |> List.rev |> find_first_digit

    let get_calibration_value chars =
      let first_digit = find_first_digit chars in
      let last_digit = find_last_digit chars in

      match first_digit, last_digit with
      | Some first_digit, Some last_digit ->
        [ first_digit; last_digit ] |> String.of_char_list |> Int.of_string
      | _ -> 0
    ;;

    let get_calibration_values lines =
      lines
      |> List.map ~f:String.to_list
      |> List.map ~f:normalize
      |> List.map ~f:get_calibration_value
    ;;

    let solve input =
      input
      |> separate_calibrations
      |> get_calibration_values
      |> List.fold ~init:0 ~f:( + )
    ;;
  end

  let run (input : string) : (string, string) result =
    Ok (input |> Trebuchet.solve |> Int.to_string)
  ;;
end
