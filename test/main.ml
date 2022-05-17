(** Testing rationale:

    comment describing your approach to testing: what you tested,
    anything you omitted testing, and why you believe that your test
    suite demonstrates the correctness of your system.

    We tested all the modules, either through OUnit testing or manually
    testing for GUI-related features. Here's what we covered:

    In this testing file, we implemented unit tests for all public
    functions of the algo, graph, road, and world modules.

    Manual testing:

    The button module does not have explicit tests because we classify
    it as a "supporting module" to controller and view, where all the
    functionality will be manually tested on the GUI itself. Buttons are
    located on the GUI, where we tested them after implementing the
    quit, load, save, edit, and edit mode buttons.

    -1: The test plan does not explain which parts of the system were
    automatically tested by OUnit vs. manually tested. -1: The test plan
    does not explain what modules were tested by OUnit and how test
    cases were developed (black box, glass box, randomized, etc.). -1:
    The test plan does not provide an argument for why the testing
    approach demonstrates the correctness of the system. *)

open OUnit2
open Pathfinder
open Graph

let pp_tuple (tuple : float * float) : string =
  match tuple with
  | x, y -> "(" ^ string_of_float x ^ "," ^ string_of_float y ^ ")"

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

let test_road_name (name : string) (expected : string) (road : Road.t) :
    test =
  name >:: fun _ -> assert_equal expected (Road.name road)

let test_road_coords
    (name : string)
    (expected : (float * float) * (float * float))
    (road : Road.t) : test =
  name >:: fun _ -> assert_equal expected (Road.road_coords road)

let floats_about_eq f1 f2 = abs_float f1 -. f2 <= 0.001

let test_road_midpt
    (name : string)
    (expected : float * float)
    (road : Road.t) : test =
  name >:: fun _ ->
  assert_equal true
    (let x1, y1 = expected in
     let x2, y2 = Road.midpt road in
     floats_about_eq x1 x2 && floats_about_eq y1 y2)
    ~printer:string_of_bool

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
    test_intersect_exists "intersection exists: regression test" true
      (108.33333333333334, 128.88888888888889)
      (108.33333333333334, 215.55555555555554)
      (101.66666666666667, 200.)
      (483.3333333333333, 200.);
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

let road1 = Road.create "Jessup Rd" (1.2, 2.2) (1.2, 2.2)
let road2 = Road.create "Triphammer Rd" (1.2, 2.2) (10.2, 4.1)
let road3 = Road.create "Main St" (4.3, 21.2) (1., 2.)

let road_name_tests =
  [
    test_road_name "road1 is called Jessup Rd" "Jessup Rd" road1;
    test_road_name "road2 is called Triphammer Rd" "Triphammer Rd" road2;
    test_road_name "road3 is called Main St" "Main St" road3;
  ]

let road_coords_tests =
  [
    test_road_coords "road1 is at (1.2, 2.2), (1.2, 2.2)"
      ((1.2, 2.2), (1.2, 2.2))
      road1;
    test_road_coords "road2 is at (1.2, 2.2), (10.2, 4.1)"
      ((1.2, 2.2), (10.2, 4.1))
      road2;
    test_road_coords "road3 is at (4.3, 21.2) (1., 2.)"
      ((4.3, 21.2), (1., 2.))
      road3;
  ]

let road_midpt_tests =
  [
    test_road_midpt "road1 midpt is at (1.2, 2.2)" (1.2, 2.2) road1;
    test_road_midpt "road2 midpt is at (5.7, 3.15)" (5.7, 3.15) road2;
    test_road_midpt "road3 midpt is at (2.65, 11.6)" (2.65, 11.6) road3;
  ]

let test_rel_and_relop string f list expected =
  string >:: fun _ ->
  match Algo.relate_option f list with
  | Some e ->
      assert_equal e expected;
      assert_equal (Algo.relate f list) expected
  | None ->
      assert_raises (Failure "No elements") (fun _ ->
          Algo.relate f list)

let scrambled =
  [ 1; 4; 2; 3; 19; 18; 6; 4; 7; -20; 9; 2; 1; 6; 2; 4; 6; 3 ]

let relate_tests =
  [
    test_rel_and_relop "get biggest" (fun x y -> x > y) scrambled 19;
    test_rel_and_relop "get smallest" (fun x y -> x < y) scrambled
    @@ -20;
    test_rel_and_relop "get first" (fun x y -> true) scrambled @@ 1;
    test_rel_and_relop "get last" (fun x y -> false) scrambled @@ 3;
  ]

let test_shortest_path
    string
    graph
    start_id
    end_id
    expected_path
    expected_distance =
  string >:: fun _ ->
  assert_equal (Algo.shortest_path start_id end_id graph) expected_path;
  assert_equal
    (Algo.distance_between start_id end_id graph)
    expected_distance

let g_12 = empty ()

let _ =
  g_12 |> add_many [ 1; 2 ];
  connect 1 2 1. g_12

let vg_12 = verify g_12
let g_137 = empty ()

let _ =
  g_137 |> add_many [ 1; 2; 3; 4; 5; 6; 7 ];
  connect 1 2 0.3 g_137;
  connect 2 4 0.3 g_137;
  connect 4 5 0.3 g_137;
  connect 5 6 0.3 g_137;
  connect 6 7 0.3 g_137;
  connect 2 5 0.5 g_137;
  connect 5 7 0.5 g_137;
  connect 1 3 0.6 g_137;
  connect 3 7 0.6 g_137

let vg_137 = verify g_137

let pathfinding_test =
  [
    test_shortest_path "simplest example" vg_12 1 2 [ 1; 2 ] 1.;
    test_shortest_path "not greedy dijkstra" vg_137 1 7 [ 1; 3; 7 ] 1.2;
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
             road_name_tests;
             road_coords_tests;
             road_midpt_tests;
             relate_tests;
             pathfinding_test;
           ])
