/datum/holder
	var/slot

/proc/entry()
	gctest()
	sleep(0)

/proc/gctest()
	var/datum/holder/h = new /datum/holder
	h.slot = "foo"+" bar"
	world.log << "pre sleep"
	sleep(0)
	world.log << h.slot
