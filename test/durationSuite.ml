open OUnit2
open Stdint

let equal = Duration.equal

let test_equal _ =
  let three = Uint64.of_int 3 in
  let four  = Uint64.of_int 4 in
  begin
    assert_equal ~cmp:equal (Duration.of_nano three) (Duration.of_nano three);
    assert_equal ~cmp:equal (Duration.of_milli three) (Duration.of_milli three);
    assert_equal ~cmp:equal (Duration.of_second four) (Duration.of_second four);
    assert_equal ~cmp:equal (Duration.of_minute four) (Duration.of_minute four);
    assert_equal ~cmp:equal (Duration.of_hour four) (Duration.of_hour four);
    assert_equal ~cmp:equal (Duration.of_day four) (Duration.of_day four);
    assert_equal ~-1 (Duration.compare (Duration.of_milli four) (Duration.of_minute three));
    assert_equal ~-1 (Duration.compare (Duration.of_second four) (Duration.of_day four));
  end

let test_to_nano _ =
  let three = Uint64.of_int 3 in  
  begin
    assert_equal (Uint64.of_string "3") (Duration.to_nano (Duration.of_nano three));
    assert_equal (Uint64.of_string "1000000") (Duration.to_nano (Duration.of_milli Uint64.one));
    assert_equal (Uint64.of_string "1000000000") (Duration.to_nano (Duration.of_second Uint64.one));
    assert_equal (Uint64.of_string "60000000000") (Duration.to_nano (Duration.of_minute Uint64.one));
    assert_equal (Uint64.of_string "3600000000000") (Duration.to_nano (Duration.of_hour Uint64.one));
    assert_equal (Uint64.of_string "86400000000000") (Duration.to_nano (Duration.of_day Uint64.one));
  end
  
let suite = "DurationSuite" >::: [
  "test_equal" >:: test_equal;
  "test_to_nano" >:: test_to_nano;
]
