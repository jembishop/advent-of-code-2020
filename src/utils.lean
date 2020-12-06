import data.vector
-- can't prove decidablity to use normal filter 

def filter {α : Type } (p : α → bool) : list α → list α
| []     := []
| (a::l) := if p a then a :: filter l else filter l

def from_list {α} {n : ℕ} : list α → option (vector α n) 
| l     := if h: l.length = n then some ⟨l, h⟩ else none
