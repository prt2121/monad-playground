import _root_.RNG.SimpleRng

val rng = SimpleRng(42)
val (n1, rng2) = rng.nextInt
val (n2, rng3) = rng2.nextInt
val (n3, rng4) = StateExercise.nonNegativeInt(rng3)
val (n4, rng5) = StateExercise.double(rng4)
1 to 10 foreach println
val (l1, rng6) = StateExercise.ints(3)(rng5)
val (l2, rng7) = StateExercise.intsRecursive(3)(rng6)
