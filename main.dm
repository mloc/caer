/proc/main()
	var/xx = abc() + 8 + abc()
	world << xx
	var/x = 7
	var/y = 3
	y = x + x + 4 + y
	world << y + 4

/proc/abc()
	var/x
	if(3)
		x = 3
		world.log << 3
	else if(7)
		x = 999
	else
		x = 39
	return x
