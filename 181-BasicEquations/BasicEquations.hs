data Line = Constant Float | Linear Float Float

instance Show Line where
	show (Constant c) = "y = " ++ show c
	show (Linear m c) = "y = " ++ show m ++ "x + " ++ show c

intersect :: Line -> Line -> (Float, Float)
intersect (Constant c) l = intersect (Linear 0 c) l
intersect l (Constant c) = intersect l (Linear 0 c)

intersect (Linear 0 x) (Linear 0 y)
	| x == y		= error "Lines are equal"
	| otherwise		= error "Lines do not intersect"

intersect (Linear m1 c1) (Linear m2 c2) =
	let x = (c1 - c2) / (m2 - m1)
	    y = m1*x + c1
	in (x, y)
