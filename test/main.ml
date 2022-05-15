open OUnit2
open Pathfinder

let test_slope
    (name : string)
    (expected : float)
    (x1 : float)
    (y1 : float)
    (x2 : float)
    (y2 : float) : test =
  name >:: fun _ -> assert_equal expected (Algo.slope x1 y1 x2 y2)

let test_distance
    (name : string)
    (expected : float)
    (p1 : float * float)
    (p2 : float * float) : test =
  name >:: fun _ -> assert_equal expected (Algo.distance p1 p2)

let test_in_range
    (name : string)
    (expected : bool)
    (p : float)
    (p1 : float)
    (p2 : float) : test =
  name >:: fun _ -> assert_equal expected (Algo.in_range p p1 p2)

let test_remove_all
    (name : string)
    (expected : 'a list)
    (list1 : 'a list)
    (list2 : 'a list) : test =
  name >:: fun _ -> assert_equal expected (Algo.remove_all list1 list2)

(** [test_intersect expected s1 f1 s2 f2] is a test that ensures that
    two roads defined by endpoints [s1]-[f1] and [s2]-[f2] have the same
    intersection as [expected] if it exists *)
let test_intersect
    (name : string)
    (expected : (float * float) option)
    (start1 : float * float)
    (finish1 : float * float)
    (start2 : float * float)
    (finish2 : float * float) : test =
  name >:: fun _ ->
  let r1 = Road.create "r1" start1 finish1 in
  let r2 = Road.create "r2" start2 finish2 in
  let inter = Road.intersection r1 r2 in
  let inter_coords =
    Option.map (fun inter -> Road.inter_coord inter) inter
  in
  assert_equal expected inter_coords

(** [test_intersect_exists expected s1 f1 s2 f2] is a test of equality
    between [expected] and the existence of the intersection of two
    roads defined by endpoints [s1]-[f1] and [s2]-[f2] *)
let test_intersect_exists
    (name : string)
    (expected : bool)
    (start1 : float * float)
    (finish1 : float * float)
    (start2 : float * float)
    (finish2 : float * float) : test =
  name >:: fun _ ->
  let r1 = Road.create "r1" start1 finish1 in
  let r2 = Road.create "r2" start2 finish2 in
  let exists = Road.intersection r1 r2 |> Option.is_some in
  assert_equal expected exists

let intersect_tests =
  [
    test_intersect
      "intersection: parallel line segments share one endpoint"
      (Some (1., 0.))
      (0., 0.) (1., 0.) (1., 0.) (2., 0.);
    test_intersect "90-degree diagonal intersection"
      (Some (1., 1.))
      (0., 0.) (2., 2.) (0., 2.) (2., 0.);
    test_intersect "90-degree vertical/horizontal intersection"
      (Some (1., 1.))
      (0., 1.) (2., 1.) (1., 0.) (1., 2.);
    test_intersect "no intersection: parallel roads" None (0., 0.)
      (1., 0.) (0., 0.1) (1., 0.1);
    test_intersect "no intersection: segments do not touch" None
      (0., 1.) (1., 1.) (2., 0.) (2., 2.);
    test_intersect "infinite intersections: overlapping roads" None
      (0., 0.) (1., 0.) (0., 0.0) (2., 0.);
    test_intersect_exists "intersection exists: misc large numbers" true
      (276., 522.) (641., 580.) (451., 706.) (610., 295.);
  ]

let slope_tests =
  [
    test_slope "slope of (1, 2) (2, 4) is 2" 2. 1. 2. 2. 4.;
    test_slope "slope of (0, 0) (3, 3) is 1" 1. 0. 0. 3. 3.;
    test_slope "slope of (0, 10) (10, 0) is -1" ~-.1. 0. 10. 10. 0.;
    test_slope "slope of (2, 100) (0, 300) is -100" ~-.100. 2. 100. 0.
      300.;
    test_slope "slope of (1, 0) (3, 3) is 1.5" 1.5 1. 0. 3. 3.;
    test_slope "slope of (1, 0) (3, 0) is 0" 0. 1. 0. 3. 0.;
  ]

let distance_tests =
  [
    test_distance "distance between (1, 0) and (4, 4) is 5" 5. (1., 0.)
      (4., 4.);
    test_distance "distance between (10, 20) and (4, 12) is 10" 10.
      (10., 20.) (4., 12.);
    test_distance "distance between (10, 0) and (15, 12) is 13" 13.
      (10., 0.) (15., 12.);
    test_distance "distance between (0, 0) and (48, 55) is 73" 73.
      (0., 0.) (48., 55.);
    test_distance "distance between (23, 100) and (43, 1) is 101" 101.
      (23., 100.) (43., 1.);
    test_distance "distance between (23, 100) and (23, 100) is 0" 0.
      (23., 100.) (23., 100.);
    test_distance "distance between (23, 100) and (23, 200) is 100" 100.
      (23., 100.) (23., 200.);
    test_distance "distance between (0, 200) and (23, 200) is 23" 23.
      (0., 200.) (23., 200.);
  ]

let in_range_tests =
  [
    test_in_range "3 is in [1, 4]" true 3. 1. 4.;
    test_in_range "3 is in [4, 1]" true 3. 4. 1.;
    test_in_range "2.21 is in [2.20, 3]" true 2.21 2.20 3.;
    test_in_range "2 is in [1, 4]" true 2. 1. 4.;
    test_in_range "10 is NOT in [1, 4]" false 10. 1. 4.;
    test_in_range "1 is in [1, 4]" true 1. 1. 4.;
    test_in_range "0 is NOT in [1, 4]" false 0. 1. 4.;
    test_in_range "-2 is in [-1, -4]" true ~-.2. ~-.1. ~-.4.;
    test_in_range "-10 is NOT in [-1, -4]" false ~-.10. ~-.1. ~-.4.;
  ]

let remove_all_tests =
  [
    test_remove_all "[] - [] = []" [] [] [];
    test_remove_all "[2] - [1] = [2]" [ 2 ] [ 2 ] [ 1 ];
    test_remove_all "[1;2] - [1] = [2]" [ 2 ] [ 1; 2 ] [ 1 ];
    test_remove_all "[1;2] - [1] = [2]" [ 2; 4; 5 ]
      [ 1; 2; 3; 4; 5; 1; 3 ] [ 1; 3 ];
    test_remove_all "[ 1; 2; 1; 1; 1; 1; 1 ] - [1] = [2]" [ 2 ]
      [ 1; 2; 1; 1; 1; 1; 1 ] [ 1 ];
    test_remove_all "[1;2] - [] = [1;2]" [ 1; 2 ] [ 1; 2 ] [];
    test_remove_all "[1;2] - [1;2] = []" [] [ 1; 2 ] [ 1; 2 ];
    test_remove_all "[1; 2; 3; 4; 5; 6] - [1; 2; 3; 4; 5; 6] = []" []
      [ 1; 2; 3; 4; 5; 6 ] [ 1; 2; 3; 4; 5; 6 ];
    test_remove_all
      "[\"adler\"; \"sean\"; \"andrew\"; \"adler\"] - [ \"sean\"; \
       \"andrew\" ] = [ \"adler\"; \"adler\" ]"
      [ "adler"; "adler" ]
      [ "adler"; "sean"; "andrew"; "adler" ]
      [ "sean"; "andrew" ];
  ]

let _ =
  run_test_tt_main
    ("test suite for final project"
    >::: List.flatten
           [
             intersect_tests;
             slope_tests;
             distance_tests;
             in_range_tests;
             remove_all_tests;
           ])

(**Test graph: let myg = empty();; let _ = add_many [1;2;3;4;5;6;7]
   myg;; let _ = connect 1 2 0.3 myg; connect 2 4 0.3 myg; connect 4 5
   0.3 myg; connect 5 6 0.3 myg; connect 6 7 0.3 myg; connect 2 5 0.5
   myg; connect 5 7 0.5 myg; connect 1 3 0.6 myg; connect 3 7 0.6 myg;;
   let myg = verify myg;; *)
