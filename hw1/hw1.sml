fun is_older (x : int*int*int, y : int*int*int) =
  let 
    fun equal (f : int*int*int -> int) = f x = f y
    fun older (f : int*int*int -> int) = f x < f y
  in
    older #1 orelse 
    (equal #1 andalso older #2) orelse 
    (equal #2 andalso older #3)
  end

fun dates_in_month (dates : (int*int*int) list, month : int) =
  let 
    fun equals_month (date : int*int*int) = #2 date = month
  in
    List.filter equals_month dates
  end

fun dates_in_months (dates : (int*int*int) list, months : int list) =
  if null months
    then []
  else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

fun number_in_month (dates : (int*int*int) list, month : int) =
  length (dates_in_month (dates, month))

fun number_in_months (dates : (int*int*int) list, months : int list) =
  if null months
    then 0
  else number_in_month (dates, hd months) + number_in_months (dates, tl months)

fun get_nth (xs : string list, n : int) =
  if n = 1
    then hd xs
  else get_nth (tl xs, n - 1)

fun date_to_string (date : int*int*int) =
  let
    val month_names =
      [ "January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November", "December"]
    val month = get_nth(month_names, #2 date)
    val day   = Int.toString (#3 date)
    val year  = Int.toString (#1 date)
  in
    month ^ " " ^ day ^ ", " ^ year
  end

fun number_before_reaching_sum (sum : int, xs : int list) =
  let
    fun number_before_reaching_sum' (xs' : int list, n : int) =
      let 
        val sumFirstTwo = hd xs' + List.nth (xs', 1) 
        val rest = List.drop(xs', 2)
      in 
        if sumFirstTwo >= sum
          then n
        else number_before_reaching_sum' (sumFirstTwo :: rest, n + 1)
      end
  in
    if hd xs >= sum
      then 0
    else number_before_reaching_sum' (xs, 1)
  end

fun what_month (x : int) =
  let val month_days = 
    [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum (x, month_days) + 1
  end

fun month_range (x : int, y : int) =
  let
    fun to_y (x' : int) = if x' > y then [] else x' :: to_y (x' + 1)
    val range = to_y x
  in
    if x > y
      then []
    else map what_month range
  end

fun oldest (dates : (int*int*int) list) =
  let
    fun oldest_first_two () = 
      let
        val first = hd dates
        val second = List.nth (dates, 1)
      in
        if is_older (first, second) then first else second
      end
    fun rest () = List.drop (dates, 2)
  in
    if null dates
      then NONE
    else if length dates = 1
      then SOME (hd dates)
    else oldest (oldest_first_two () :: rest ())
  end