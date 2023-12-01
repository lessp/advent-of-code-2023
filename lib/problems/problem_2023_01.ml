let year = 2023
let day = 1

module Part_1 = struct
  module Trebuchet = struct
    open Base

    let separate_calibrations input = input |> String.split ~on:'\n'

    let find_first_digit chars =
      chars |> List.find ~f:Char.is_digit |> Option.map ~f:Char.to_string
    ;;
    let find_last_digit chars = chars |> List.rev |> find_first_digit

    let get_calibration_value chars =
      let first_digit = find_first_digit chars in
      let last_digit = find_last_digit chars in

      match first_digit, last_digit with
      | Some first_digit, Some last_digit -> first_digit ^ last_digit |> Int.of_string
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
      | 'o' :: 'n' :: 'e' :: tail -> Some ('1', (* -> *) 'e' (* <- *) :: tail)
      | 't' :: 'w' :: 'o' :: tail -> Some ('2', 'o' :: tail)
      | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: tail -> Some ('3', 'e' :: tail)
      | 'f' :: 'o' :: 'u' :: 'r' :: tail -> Some ('4', 'r' :: tail)
      | 'f' :: 'i' :: 'v' :: 'e' :: tail -> Some ('5', 'e' :: tail)
      | 's' :: 'i' :: 'x' :: tail -> Some ('6', 'x' :: tail)
      | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: tail -> Some ('7', 'n' :: tail)
      | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: tail -> Some ('8', 't' :: tail)
      | 'n' :: 'i' :: 'n' :: 'e' :: tail -> Some ('9', 'e' :: tail)
      | _ -> None
    ;;

    let find_first_digit chars =
      chars |> List.find ~f:Char.is_digit |> Option.map ~f:Char.to_string
    ;;

    let find_last_digit chars =
      let result = chars |> List.rev |> find_first_digit in
      result
    ;;

    let get_calibration_value chars =
      let first_digit = find_first_digit chars in
      let last_digit = find_last_digit chars in

      match first_digit, last_digit with
      | Some first_digit, Some last_digit -> first_digit ^ last_digit |> Int.of_string
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
