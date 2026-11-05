val failures = ref 0

fun check name ok =
    if ok then
        print ("PASS " ^ name ^ "\n")
    else (
        failures := !failures + 1;
        print ("FAIL " ^ name ^ "\n")
    )

val seconds = 60

fun area r = Math.pi * r * r

(* 練習2.1 *)

(* 練習2.2
不同意，實數=和整數意義都不一樣
*)

(* 練習2.3
1和3要，2不需要，要有没有用到重載的函數或者操作符
*)

(* *重載 *)
fun double n = 2 * n

fun f u = Math.sin u / u

(* ~和*重載 *)
fun g k = ~ k * k

(* 練習2.4
第一個版本：~1返回前面那個數，10返回非數字
實際上都是非數字
第二個版本：~1報錯，10越界
*)

fun digit i = chr(i + ord #"0")

fun digit i = String.sub("0123456789", i)

(* 練習2.5 *)

fun is_date (d, m) =
    if m = "January" orelse m = "March" orelse m = "May" orelse
        m = "July" orelse m = "August" orelse m = "October" orelse
        m = "December" then
        1 <= d andalso d <= 31
    else if m = "April" orelse m = "June" orelse m = "September" orelse
        m = "November" then
        1 <= d andalso d <= 30
    else m = "February" andalso (1 <= d andalso d <= 28)

val zerovec = (0.0, 0.0)

fun lengthvec (x, y) = Math.sqrt (x * x + y * y)

fun negvec (x, y) : real * real = (~ x, ~ y)

type vec = real * real

fun addvec ((x1, y1), (x2, y2)) : vec = (x1 + x2, y1 + y2)

fun subvec (v1, v2) = addvec (v1, negvec v2)

fun distance pairv = lengthvec (subvec pairv)

(* ---- 測試用例 ---- *)
val () = check "31 January true"   (is_date (31, "January"))
val () = check "31 April false"     (not (is_date (31, "April")))
val () = check "29 February false"  (not (is_date (29, "February")))
val () = check "0 March false"      (not (is_date (0, "March")))

(* 練習2.6 *)

type time = (int * int * string)

fun time12to24 (h, m, p) =
    (* 凌晨十二點算零點 *)
    if p = "AM" andalso h = 12 then
        (0, m)
    (* 12:00PM至1:00PM之間需特殊處理 *)
    else if (p = "AM") orelse (h = 12 andalso p = "PM") then
        (h, m)
    else if p = "PM" then
        (h + 12, m)
    else
        (h, m)

fun timeBeforeHelp (ah, am) (bh, bm) =
    (ah < bh) orelse ((ah = bh) andalso (am < bm))

fun timeBefore a b =
    timeBeforeHelp (time12to24 a) (time12to24 b) 

(* ---- timeBefore 測試 ---- *)
val () = check "10:00AM < 11:00AM"          (timeBefore (10,0,"AM") (11,0,"AM"))
val () = check "11:00AM < 10:00AM = false"  (not (timeBefore (11,0,"AM") (10,0,"AM")))
val () = check "10:30AM < 10:45AM"          (timeBefore (10,30,"AM") (10,45,"AM"))
val () = check "10:45AM < 10:30AM = false"  (not (timeBefore (10,45,"AM") (10,30,"AM")))
val () = check "10:30AM < 10:30AM = false"  (not (timeBefore (10,30,"AM") (10,30,"AM")))
val () = check "11:59AM < 12:00PM"          (timeBefore (11,59,"AM") (12,0,"PM"))
val () = check "1:00PM < 1:00AM = false"    (not (timeBefore (1,0,"PM") (1,0,"AM")))
val () = check "11:59PM < 12:00AM = false"  (not (timeBefore (11,59,"PM") (12,0,"AM")))
val () = check "12:00AM < 12:00PM"          (timeBefore (12,0,"AM") (12,0,"PM"))
val () = check "12:00PM < 12:00AM = false"  (not (timeBefore (12,0,"PM") (12,0,"AM")))
val () = check "12:30AM < 1:00AM"           (timeBefore (12,30,"AM") (1,0,"AM"))
val () = check "12:30PM < 1:00PM"           (timeBefore (12,30,"PM") (1,0,"PM"))
val () = check "12:00PM < 11:59PM"          (timeBefore (12,0,"PM") (11,59,"PM"))
val () = check "1:00PM < 12:30PM = false"   (not (timeBefore (1,0,"PM") (12,30,"PM")))
val () = check "12:00AM < 12:01AM"          (timeBefore (12,0,"AM") (12,1,"AM"))

(* 有失敗就以非零退出碼結束，方便腳本判斷 *)
val () = if !failures = 0 then () else OS.Process.exit OS.Process.failure
