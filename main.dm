/proc/main()
	var/xx = abc() + 8 + abc()
	world << xx
	var/x = 7
	var/y = 3
	y = x + x + 4 + y
	world << y + 4

/proc/abc()
	world << 3
	return 3
