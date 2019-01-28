/proc/main()
	//var/xx = abc() + 8 + abc()
	//world << xx
	//var/x = 7
	//var/y = 3
	//y = x + x + 4 + y
	//world << loopy()
	world  << simple()
	//world << y + 4

/proc/abc()
	var/x
	if(3)
		x = 3
		world.log << 3
	else if(7)
		x = 999
	else
		x = 39
	world.log << 78
	return x

/proc/loopy()
	var/x = 0 + 8 + 3.4
	while(x + 8)
		x = x - 1
	return x

/proc/simple()
	var/x
	if(1)
		x = 3
	else
		x = 4
	return x
