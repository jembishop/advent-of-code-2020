
-- can't prove decidablity to use normal filter 
def filter {α : Type } (p : α → bool) : list α → list α
| []     := []
| (a::l) := if p a then a :: filter l else filter l