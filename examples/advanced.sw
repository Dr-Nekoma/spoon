[[lambda
	[(x Integer)
	 (y Boolean)
	 (z (-> (Integer Integer) Integer))
	 &(numbers Integer)]	
	[if y [z x 2] [+ numbers]]]
	[if [or F F F F]
	    [+ 1 2 3 4]
	    [- 1 2 3 4]]
	[not [and [< 3 4] [> 5 10]]]
	[lambda [(a Integer) (b Integer)] [+ a b]]
	1 2 3 4 5]

