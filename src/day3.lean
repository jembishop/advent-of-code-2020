import system.io 
import data.vector
import data.zmod.basic
import utils
open io
open list


def tree_p : char → option bool 
| '#' := some true
| '.' := some false
| _   := none

def WIDTH : ℕ := 31

def tree_line := vector bool WIDTH 

def parse_line : string → option tree_line
| s     := do
    let mapped : list (option bool) := tree_p <$> s.data,
    uw ← (monad.sequence mapped),
    from_list uw        


def parse : string → option (list tree_line)
| s     := monad.sequence (functor.map parse_line (list.filter (≠"") (s.split (='\n'))))

instance : has_to_string tree_line
:= ⟨λ x: tree_line, x.val.to_string⟩ 

instance {n : ℕ} : has_to_string (zmod n) := ⟨λ x , to_string x.val⟩ 

def is_tree : ℕ → tree_line → bool 
| n tl  := let modW : zmod WIDTH := n in tl.nth modW 

def enumerate_h {α} : ℕ → list α → list (ℕ × α)
| n []  := []
| n (x::xs)  := (n, x) :: (enumerate_h (n + 1) xs)

def enumerate {α} : list α → list (prod ℕ α) 
| l := enumerate_h 0 l 

def position : ℕ → ℕ → ℕ 
| grad y       := grad*y

def hits : ℕ × ℕ → list tree_line → ℕ 
| (step, grad) tls   := length $ filter id $ map 
(λt: prod ℕ tree_line, 
is_tree (position grad (t.1/step)) t.2 ) 
(filter (λt : prod ℕ tree_line, t.1 % step = 0) (enumerate tls))

def productl : list ℕ → ℕ := list.foldr (*) 1

def many_hits : list (ℕ × ℕ) → list tree_line → ℕ 
| hs tls := productl $ map (λ h, hits h tls) hs

def main : io unit := do
    contents ← fs.read_file "inputs/day3.txt",
    let parsed := parse contents.to_string,
    put_str_ln $ to_string $ (hits (1, 3)) <$> parsed,
    put_str_ln $ to_string $ 
    (many_hits [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]) <$> parsed