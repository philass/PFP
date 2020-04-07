-- We represent a spin as a single byte.  In principle, we need only
-- two values (-1 or 1), but Futhark represents booleans a a full byte
-- entirely, so using an i8 instead takes no more space, and makes the
-- arithmetic simpler.
type spin = i8

import "lib/github.com/diku-dk/cpprandom/random"

-- Pick an RNG engine and define random distributions for specific types.
module rng_engine = minstd_rand
module rand_f32 = uniform_real_distribution f32 rng_engine
module rand_i8 = uniform_int_distribution i8 rng_engine

-- We can create an few RNG state with 'rng_engine.rng_from_seed [x]',
-- where 'x' is some seed.  We can split one RNG state into many with
-- 'rng_engine.split_rng'.
--
-- For an RNG state 'r', we can generate random integers that are
-- either 0 or 1 by calling 'rand_i8.rand (0i8, 1i8) r'.
--
-- For an RNG state 'r', we can generate random floats in the range
-- (0,1) by calling 'rand_f32.rand (0f32, 1f32) r'.
--
-- Remember to consult https://futhark-lang.org/docs/futlib/random.html

let rand = rand_f32.rand (0f32, 1f32)

-- Create a new grid of a given size.  Also produce an identically
-- sized array of RNG states.
let random_grid (seed: i32) (h: i32) (w: i32)
              : ([h][w]rng_engine.rng, [h][w]spin) = 
	      let rng_state = rng_engine.rng_from_seed [seed]
	      let states = rng_engine.split_rng (h * w) rng_state
	      let pair_spins = map (\r -> rand_i8.rand (0i8, 1i8) r) states
	      let spins = map (\(_, v) -> if v == 0 then -1 else 1) pair_spins
	      in (unflatten h w states, unflatten h w spins)
--
---- Compute $\Delta_e$ for each spin in the grid, using wraparound at
---- the edges.
let deltas [h][w] (spins: [h][w]spin): [h][w]i8 = 
	let u_val r c = if r == 0   then spins[h-1,c  ] else spins[r - 1, c]
	let d_val r c = if r == h-1 then spins[0  ,c  ] else spins[r + 1, c]
	let l_val r c = if c == 0   then spins[r  ,w-1] else spins[r , c - 1]
	let r_val r c = if c == w-1 then spins[r  ,0  ] else spins[r , c + 1]
	let f r c = 2  * spins[r, c] * ((u_val r c) + (d_val r c) + (l_val r c) + (r_val r c))
	in tabulate_2d h w f

--
-- The sum of all deltas of a grid.  The result is a measure of how
-- ordered the grid is.
let delta_sum [h][w] (spins: [w][h]spin): i32 =
  deltas spins |> flatten |> map1 i32.i8 |> i32.sum

-- Take one step in the Ising 2D simulation.

let helper (r: rng_engine.rng) (spin: spin) (abs_temp: f32) (samplerate: f32) (e: i32) (e_grad: i8) = 
	let (r1, a) = rand_f32.rand (0f32, 1f32) r
	let (r2, b) = rand_f32.rand (0f32, 1f32) r1
	let test = a < samplerate && ((e_grad < (-e_grad)) || b < (f32.i32 e) ** ((f32.i8 (-e_grad) / abs_temp)))
	in if test then (r2, (-spin)) else (r2, spin)

let step [h][w] (abs_temp: f32) (samplerate: f32)
                (rngs: [h][w]rng_engine.rng) (spins: [h][w]spin)
              : ([h][w]rng_engine.rng, [h][w]spin) =
	let delta_table = deltas spins
	let e = delta_sum spins
	let result = map3 (map3 (\egrad spin r -> helper r spin abs_temp samplerate e egrad)) delta_table spins rngs
	let f_result = flatten result
	let (r_vals, spinnys) = unzip f_result
	in (unflatten h w r_vals, unflatten h w spinnys)
	
	

import "lib/github.com/athas/matte/colour"

-- | Turn a grid of spins into an array of pixel values, ready to be
-- blitted to the screen.
let render [h][w] (spins: [h][w]spin): [h][w]argb.colour =
  let pixel spin = if spin == -1i8
                   then argb.(bright <| light red)
                   else argb.(bright <| light blue)
  in map1 (map1 pixel) spins

-- | Just for benchmarking.
let main (abs_temp: f32) (samplerate: f32)
         (h: i32) (w: i32) (n: i32): [h][w]spin =
  (loop (rngs, spins) = random_grid 1337 h w for _i < n do
     step abs_temp samplerate rngs spins).1
