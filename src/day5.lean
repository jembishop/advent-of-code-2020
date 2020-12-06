import system.io
import data.vector
import data.list.sort
import utils
open io
open monad

def seat_id := (vector bool 7) × (vector bool 3)

instance : has_to_string seat_id := 
⟨λ x: seat_id, "row:" ++ to_string x.1.val ++ " col:" ++ to_string x.2.val⟩ 

def bool_p : char × char →  char → option bool
| (c1, c2) c := if c=c1 then some true else if c=c2 then some false else none


def parse_seat :string → option seat_id
| s     := let p := s.data.span (λ x, x='B' ∨ x='F') in 
        do 
        l1 ← monad.sequence (p.1.map (bool_p ('B', 'F'))),
        l2 ← monad.sequence (p.2.map (bool_p ('R', 'L'))),
        v1 ← @from_list bool 7 l1,
        v2 ← @from_list bool 3 l2,
        return (v1, v2)

def parse : string → option (list seat_id)
| s := monad.sequence $ list.map parse_seat (filter (≠"") (s.split (='\n')))

instance : has_le bool := ⟨(→)⟩ 

def lte_seat_id_h : list bool → list bool → bool
| [] [] := true
| [] a  := true
| a []  := false
| (x :: xs) (y :: ys) := if x = y then lte_seat_id_h xs ys else x ≤ y

def lte_seat_id :seat_id → seat_id → bool
| s1 s2 := let s1m := (vector.append s1.1 s1.2).val,
               s2m := (vector.append s2.1 s2.2).val
               in lte_seat_id_h s1m s2m

def sort_ids : list seat_id → list seat_id 
| ls :=  list.insertion_sort (λ x y , ¬(lte_seat_id x y)) ls 

def highest : list seat_id → option seat_id 
| ls    := match sort_ids ls with
    | [] := none
    | (x::xs) := x
    end

def suml : list ℕ → ℕ := list.foldr (+) 0

def to_num : ℕ → list bool → ℕ 
| offset lb     := suml $
 lb.enum.map 
 (λ p : (ℕ × bool), if p.2 then 2^(offset - p.1) else 0)

def seat_to_num : seat_id → ℕ
| (r, c) := (to_num 9 r.val) + (to_num 2 c.val)

def diffs : list ℕ → list ℕ 
|[] := []
|[x] := []
|(x::y::xs) := (x - y) :: (diffs (y::xs))

def find_missing : list ℕ → ℕ 
| l     := (list.find_index (≠1) (diffs l)) + 1


def main : io unit := do
    contents ← fs.read_file "inputs/day5.txt",
    let parsed := parse contents.to_string,
    put_str_ln $ to_string $ do { 
            l ← parsed, 
            h ← highest l,
            (some (seat_to_num h))},

    put_str_ln $ to_string $ do 
            l ← parsed, 
            let ns := (seat_to_num <$> (sort_ids l)), 
            let n := find_missing ns, 
            v ← ns.nth n,
            return (v + 1)