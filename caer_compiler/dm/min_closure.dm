/proc/entry()
	world.log << "before spawn"
	var/x = "captured"
	var/y = "big"
	spawn
		world.log << "in spawn: [y] [x]"
	world.log << "after spawn"
