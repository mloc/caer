/proc/main()
	var/xx = abc() + 8 + abc()
	world << xx
	var/x = 7
	var/y = 3
	y = x + x + 4 + y
	world << y + 4

/proc/abc()
	if(3)
		world << 3.14
	else if(7)
		world << 999
	else
		world << 382
	return 3
