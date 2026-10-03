-- Rectify.Optimization.TSP
-- Traveling Salesman Problem (pure domain)

import Rectify.Optimization
import Rectify.Optimization.SA
import Lean.Data.Json

namespace Rectify.Optimization.TSP

open Rectify.Optimization

-- ============================================================================
-- Domain types
-- ============================================================================

def Float.pi : Float := 3.14159265358979323846

structure Point2D where
  x : Float
  y : Float
  deriving Repr, BEq, Inhabited

instance : Lean.ToJson Point2D where
  toJson p := Lean.Json.mkObj [("x", Lean.toJson p.x), ("y", Lean.toJson p.y)]

-- ============================================================================
-- Cities
-- ============================================================================

inductive City where
  | Helsinki | Espoo | Tampere | Vantaa | Oulu
  | Turku | Jyvaskyla | Lahti | Kuopio | Pori
  | Kouvola | Joensuu | Lappeenranta | Hameenlinna | Vaasa
  | Seinajoki | Rovaniemi | Mikkeli | Kotka | Salo
  | Porvoo | Kokkola | Hyvinkaa | Lohja | Jarvenpaa
  | Rauma | Kajaani | Kerava | Savonlinna | Nokia
  deriving Repr, BEq, DecidableEq, Inhabited

def City.coords : City → Point2D
  | .Helsinki      => ⟨60.166641, 24.943537⟩
  | .Espoo         => ⟨60.206376, 24.656729⟩
  | .Tampere       => ⟨61.497743, 23.761290⟩
  | .Vantaa        => ⟨60.298134, 25.006641⟩
  | .Oulu          => ⟨65.013785, 25.472099⟩
  | .Turku         => ⟨60.451690, 22.266867⟩
  | .Jyvaskyla     => ⟨62.241678, 25.749498⟩
  | .Lahti         => ⟨60.980381, 25.654988⟩
  | .Kuopio        => ⟨62.892983, 27.688935⟩
  | .Pori          => ⟨61.483726, 21.795900⟩
  | .Kouvola       => ⟨60.866825, 26.705598⟩
  | .Joensuu       => ⟨62.602079, 29.759679⟩
  | .Lappeenranta  => ⟨61.058750, 28.187690⟩
  | .Hameenlinna   => ⟨60.996174, 24.464425⟩
  | .Vaasa         => ⟨63.092589, 21.615874⟩
  | .Seinajoki     => ⟨62.786663, 22.842280⟩
  | .Rovaniemi     => ⟨66.502790, 25.728479⟩
  | .Mikkeli       => ⟨61.687727, 27.273224⟩
  | .Kotka         => ⟨60.465521, 26.941153⟩
  | .Salo          => ⟨60.384374, 23.126727⟩
  | .Porvoo        => ⟨60.395372, 25.666560⟩
  | .Kokkola       => ⟨63.837583, 23.131962⟩
  | .Hyvinkaa      => ⟨60.631017, 24.861124⟩
  | .Lohja         => ⟨60.250916, 24.065782⟩
  | .Jarvenpaa     => ⟨60.481098, 25.100747⟩
  | .Rauma         => ⟨61.128738, 21.511127⟩
  | .Kajaani       => ⟨64.226734, 27.728047⟩
  | .Kerava        => ⟨60.404869, 25.103549⟩
  | .Savonlinna    => ⟨61.869803, 28.878498⟩
  | .Nokia         => ⟨61.478774, 23.508499⟩

def allCities : Array City := #[
  .Helsinki, .Espoo, .Tampere, .Vantaa, .Oulu,
  .Turku, .Jyvaskyla, .Lahti, .Kuopio, .Pori,
  .Kouvola, .Joensuu, .Lappeenranta, .Hameenlinna, .Vaasa,
  .Seinajoki, .Rovaniemi, .Mikkeli, .Kotka, .Salo,
  .Porvoo, .Kokkola, .Hyvinkaa, .Lohja, .Jarvenpaa,
  .Rauma, .Kajaani, .Kerava, .Savonlinna, .Nokia
]

/-- Distance between two cities in km (approximate) -/
def City.dist (a b : City) : Float :=
  let pa := a.coords
  let pb := b.coords
  let dlat := (pb.x - pa.x) * 111.0
  let dlon := (pb.y - pa.y) * 111.0 * Float.cos (pa.x * Float.pi / 180.0)
  Float.sqrt (dlat * dlat + dlon * dlon)

-- ============================================================================
-- Tour (solution type)
-- ============================================================================

/-- A tour is a permutation of cities -/
structure Tour where
  cities : Array City
  deriving Repr, Inhabited

/-- Total tour distance (returning to start) -/
def Tour.distance (tour : Tour) : Float :=
  if tour.cities.size < 2 then 0.0 else
    let n := tour.cities.size
    let pairDist := (List.range (n - 1)).foldl (init := 0.0) fun acc i =>
      acc + City.dist tour.cities[i]! tour.cities[i + 1]!
    pairDist + City.dist tour.cities[n - 1]! tour.cities[0]!

instance : Lean.ToJson Tour where
  toJson tour :=
    let scaled := tour.cities.map fun c =>
      let p := c.coords
      Point2D.mk ((p.x - 63.0) * 50.0) ((p.y - 25.0) * 30.0)
    Lean.Json.mkObj [
      ("tag", "TSPSolution"),
      ("cities", Lean.toJson scaled.toList)
    ]

-- ============================================================================
-- TSP as a Problem
-- ============================================================================

def problem : Problem Tour where
  fitness := Tour.distance

-- ============================================================================
-- SA Adapter for TSP
-- ============================================================================

/-- Fisher-Yates shuffle -/
def shuffle (arr : Array α) [Inhabited α] : RandomM (Array α) := do
  let mut a := arr
  for i in [1:a.size] do
    let j ← randNatRange 0 i
    a := a.swapIfInBounds i j
  pure a

/-- 2-opt neighbor: swap two cities -/
def twoOptNeighbor (tour : Tour) : RandomM Tour := do
  let n := tour.cities.size
  if n < 2 then return tour
  let i ← randNatRange 0 (n - 1)
  let j ← randNatRange 0 (n - 1)
  pure ⟨tour.cities.swapIfInBounds i j⟩

/-- SA adapter for TSP using 2-opt -/
def saAdapter : SA.Adapter Tour where
  initial := do
    let shuffled ← shuffle allCities
    pure ⟨shuffled⟩
  fitness := Tour.distance
  neighbor := twoOptNeighbor

/-- Default SA parameters tuned for TSP -/
def saParams : SA.Params where
  initialTemp := 1000.0
  finalTemp := 0.1
  coolingRate := 0.9997

end Rectify.Optimization.TSP
