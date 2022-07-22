/datum/holder
	var/x

/proc/entry()
	var/datum/holder/h = new /datum/holder

	update(h)
	process(h)

/proc/update(var/datum/holder/h)
	h.x = "hello, world"

/proc/process(var/datum/holder/h)
	world.log << h.x
