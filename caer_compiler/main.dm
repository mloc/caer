/datum/container
	var/obj/obj

/datum/container/proc/foo()
	return "hi am foo"

/proc/argfoo(var/bar)
	world << bar

/proc/entry()
	/*
	world << (3 & 6)
	var/s = "hey"
	world << "[s] there[1/90]"
	argfoo(s)

	var/datum/container/c = null
	c = new /datum/container
	c.obj = new /obj
	c.obj.name = "hello there"
	world << "NAME: [c.obj.name]"
	var/msg = c.foo()
	world << msg

	world << loopy()

	var/v = 0 || 80
	world << v

	world << 2 ** 7
	var/pflo = 2 ** 7
	world << pflo
	var/b = pflo - pflo + (0-1)
	world << b
	var/band = b & b
	world << band
	world << bitmess(band)
	*/

	world << abc()

	var/x = 3
	x = "hey"

	world << x

	//world << (c:foo)
	//testlists()

	catchet()

	//world << 1 + 1
	/*world << simple()
	var/xx = abc() + 8 + abc()
	world << xx
	var/x = 7
	var/y = 3
	world << y
	y = x + x + 4 + y
	world << loopy()
	world << y + 4
	forl()
*/
/proc/abc()
	var/x
	if(3)
		//var/y
		x = 3
		world.log << 3
		spawn
	else if(7)
		x = 999
	else
		if(0)
			x = 39
	world.log << 78
	return x


/proc/loopy()
	var/x = 0
	while(x > 0-8)
		x = x - 1
		world << x
	return x

/proc/bitmess(x)
	return (x & 3829) | (x ^ 57743)

/proc/testlists()
	var/list/rec = new /list
	rec:Add(1, 2, rec, 3, 4, rec, 5)
	world << "rec l: [rec]"

	var/list/l1 = new /list
	l1:Add(1, 2, 3, 4)
	var/list/l2 = l1:Copy()
	l1:Add(5)
	world << "l1: [l1]"
	world << "l2: [l2]"
	world << "l1.Copy(3): [l1:Copy(3)]"
	world << "l1.Copy(2, 4): [l1:Copy(2, 4)]"

/proc/catchet()
	world << "before try"
	try
		try
			world << "inside try, before throw"
			throw 4
			world << "inside try, after throw"
		catch(var/x)
			world << "in inner catch, x: [x]"
			throw x
	catch(var/x)
		world << "in outer catch, x: [x]"
	world << "after try"
/*
/proc/simple()
	var/x
	if(1)
		x = 3
	else
		x = 4
	return x

/proc/forl()
	for(var/i = 1; i - 4; i = i + 1)
		for(var/j = 1; j - 4; j = j + 1)
			world << i + j*/

//#include "badty.dm"
