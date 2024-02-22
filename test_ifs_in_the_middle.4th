2 2 + if 1 else 2 ; 0 if 1 else 7 ; 20 if 30 ; 0 if 40 ; 5

/' expected output 
Expression [Terminal (Val 2.0),Terminal (Val 2.0),Terminal (Word "+"),If {ifTrue = Expression [Terminal (Val 1.0)], ifFalse = Expression [Terminal (Val 2.0)]},Terminal (Val 0.0),If {ifTrue = Expression [Terminal (Val 1.0)], ifFalse = Expression [Terminal (Val 7.0)]},Terminal (Val 20.0),If {ifTrue = Expression [Terminal (Val 30.0)], ifFalse = Expression []},Terminal (Val 0.0),If {ifTrue = Expression [Terminal (Val 40.0)], ifFalse = Expression []},Terminal (Val 5.0)]

[4.0,1.0,0.0,7.0,20.0,30.0,0.0,5.0]
'/