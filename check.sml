(* check.sml — 共用測試 harness *)
structure Check = struct
  (* 是否印出 PASS；由 runner 設定 *)
  val verbose = ref false

  val failures = ref 0

  fun check name ok =
    if ok then
      (if !verbose then print ("PASS " ^ name ^ "\n") else ())
    else (
      failures := !failures + 1;
      print ("FAIL " ^ name ^ "\n")
    )

  fun reset () = failures := 0

  fun done () =
    if !failures = 0 then ()
    else OS.Process.exit OS.Process.failure
end
