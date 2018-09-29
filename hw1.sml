fun is_older (date1 : int*int*int, date2 : int*int*int) =
    if (#1 date1) <> (#1 date2) then (#1 date1) < (#1 date2)
    else if (#2 date1) <> (#2 date2) then (#2 date1) < (#2 date2)
    else (#3 date1) < (#3 date2)

fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates then 0
    else let
        val count = if month = (#2 (hd dates)) then 1 else 0    
    in
        count + number_in_month(tl dates, month)
    end

fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates then []
    else if month = (#2 (hd dates))
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)                

fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (vals : 'a list, n : int) =
    if n = 1 then hd vals
    else get_nth(tl vals, n-1)

fun date_to_string (date : int*int*int) =
    let
        val year = Int.toString (#1 date)
        val month = get_nth(["January","February","March","April","May","June","July","August","September","October","November","December"], (#2 date))
        val day = Int.toString (#3 date)                       
    in
        month^" "^day^", "^year
    end

fun number_before_reaching_sum (sum : int, ns : int list) =
    let
        val n = hd ns
    in
        if n >= sum then 0
        else 1 + number_before_reaching_sum (sum - n, tl ns)
    end                                     

fun what_month (day : int) =
    number_before_reaching_sum(day, [31,28,31,30,31,30,31,31,30,31,30,31])

fun month_range (day1 : int, day2 : int) =
    if day1 >= day2 then []
    else what_month(day1) :: month_range(day1+1, day2)

fun oldest (dates : (int*int*int) list) =
    if null dates then NONE
    else let
        val old_max = oldest(tl dates)
    in
        if isSome old_max andalso is_older(valOf old_max, hd dates) then old_max
        else SOME (hd dates)
    end

fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
    let
        fun remove_dups (ns : int list) =
            if null ns then []
            else let
                fun remove_num (num : int, ns : int list) =
                    if null ns then []
                    else let
                        val old_ns = remove_num(num, tl ns)                     
                    in
                        if num = (hd ns) then old_ns
                        else (hd ns) :: old_ns
                    end                             
            in
                hd ns :: remove_dups(remove_num(hd ns, tl ns))
            end    
    in
        number_in_months(dates, remove_dups(months))
    end

fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
    let
        fun member_of (num : int, ns : int list) =
            if null ns then false
            else if num = (hd ns) then true
            else member_of(num, tl ns)
        fun remove_dups (ns : int list) =
            if null ns then []
            else let
                val old_ns = remove_dups(tl ns)
            in
                if member_of(hd ns, old_ns) then old_ns
                else hd ns :: old_ns
            end
    in
        dates_in_months(dates, remove_dups(months))
    end      

fun reasonable_date (date : int*int*int) =
    let
        val check_year = (#1 date) > 0
        val check_month = 0 < (#2 date) andalso (#2 date) <= 12
        val check_day =
            let
                fun leap_year (year : int) =
                    if year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0) then true
                    else false
                val feb = if leap_year (#1 date) then 29 else 28
                val days = [31,feb,31,30,31,30,31,31,30,31,30,31]
            in
                0 < (#3 date) andalso (#3 date) <= get_nth(days, (#2 date))
            end                                  
    in
        check_year andalso check_month andalso check_day
    end
                                 
                             
