open OUnit2
open Pathfinder

(** [test_intersect expected s1 f1 s2 f2] is a test that ensures that two roads
    defined by endpoints [s1]-[f1] and [s2]-[f2] have the same intersection as
    [expected] if it exists *)
let test_intersect (name : string) (expected : (float * float) option)
    (start1 : float * float) (finish1 : float * float)
    (start2 : float * float) (finish2 : float * float)
    : test =
    name >:: fun _ ->
    let r1 = Road.create "r1" start1 finish1 in
    let r2 = Road.create "r2" start2 finish2 in
    let inter = Road.intersection r1 r2 in
    let inter_coords = Option.map (fun inter -> Road.inter_coord inter) inter in
    assert_equal expected inter_coords

(** [test_intersect_exists expected s1 f1 s2 f2] is a test of equality
    between [expected] and the existence of the intersection of two roads
    defined by endpoints [s1]-[f1] and [s2]-[f2] *)
let test_intersect_exists (name : string) (expected : bool)
    (start1 : float * float) (finish1 : float * float)
    (start2 : float * float) (finish2 : float * float)
    : test =
    name >:: fun _ ->
    let r1 = Road.create "r1" start1 finish1 in
    let r2 = Road.create "r2" start2 finish2 in
    let exists = Road.intersection r1 r2 |> Option.is_some in
    assert_equal expected exists

let intersect_tests = "intersection test suite" >::: [
    test_intersect "intersection: parallel line segments share one endpoint"
        (Some (1., 0.)) (0., 0.) (1., 0.) (1., 0.) (2., 0.);
    test_intersect "90-degree diagonal intersection"
        (Some (1., 1.)) (0., 0.) (2., 2.) (0., 2.) (2., 0.);
    test_intersect "90-degree vertical/horizontal intersection"
        (Some (1., 1.)) (0., 1.) (2., 1.) (1., 0.) (1., 2.);
    test_intersect "no intersection: parallel roads"
        None (0., 0.) (1., 0.) (0., 0.1) (1., 0.1);
    test_intersect "no intersection: segments do not touch"
        None (0., 1.) (1., 1.) (2., 0.) (2., 2.);
    test_intersect "infinite intersections: overlapping roads"
        None (0., 0.) (1., 0.) (0., 0.0) (2., 0.);
    test_intersect_exists "intersection exists: misc large numbers"
        true (276., 522.) (641., 580.) (451., 706.) (610., 295.);
]

let _ = run_test_tt_main intersect_tests
