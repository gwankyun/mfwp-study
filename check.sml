(* check.sml — 共用測試 harness *)
structure Check = struct
  (* 是否印出 PASS；由 runner 設定 *)
  val verbose = ref false

  val failures = ref 0
  val checks   = ref 0

  fun check name ok = (
    checks := !checks + 1;
    if ok then
      (if !verbose then print ("PASS " ^ name ^ "\n") else ())
    else (
      failures := !failures + 1;
      print ("FAIL " ^ name ^ "\n")
    )
  )

  fun reset () = (failures := 0; checks := 0)

  fun done () =
    if !failures = 0 then
      print ("ALL TESTS PASSED (" ^ Int.toString (!checks) ^ " checks)\n")
    else (
      print ("FAILED: " ^ Int.toString (!failures) ^ " of "
             ^ Int.toString (!checks) ^ " checks\n");
      OS.Process.exit OS.Process.failure
    )
end
