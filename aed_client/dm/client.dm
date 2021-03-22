#define LIB_PATH "libmclient.so"

/world/New()
	world.log << "starting"
	call(LIB_PATH, "dibby_setup")()
	var/msg = json_encode(list("Message" = "hallo"))
	call(LIB_PATH, "dibby_send")(msg)
	while(TRUE)
		var/ret = call(LIB_PATH, "dibby_recv")()
		world.log << json_encode(ret)
		if(ret != "")
			break
		sleep(10)
	call(LIB_PATH, "dibby_shutdown")()
	del(src)

#define GET_OBJECT(id) (_all_objects[id])
#define SET_OBJECT(id, obj) _all_objects[id] = obj
/var/_all_objects[16777416]

#define POOL_SIZE 10000
#define CREATE_OBJECT (_pool_head? (_pooled_objects[_pool_head--]) : (new /obj/game_object))
#define POOL_OBJECT(obj) if(_pool_head < POOL_SIZE) {_pooled_objects[++_pool_head] = obj}
/var/_pool_head = 0
/var/_pooled_objects[POOL_SIZE]

/obj/game_object
	var/id

/datum/manager

//TODO: macroify
/proc/process_message(msg)
	var/key = msg[1]
	var/list/value = msg[key]

	switch(key)
		if("Message")
			world << value
		if("UpdateObject")
			var/id = value["id"]
			var/obj/game_object/obj = GET_OBJECT(id)
			if(obj == null)
				obj = CREATE_OBJECT
				obj.id = id
				SET_OBJECT(id, obj)
			sync_object(obj, value)
		if("DelObject")
			var/id = value["id"]
			var/obj/game_object/obj = GET_OBJECT(id)
			if(obj != null)
				obj.loc = null
				obj.appearance = null
				SET_OBJECT(id, null)
				POOL_OBJECT(obj)

//TODO: optimize var use
//TODO: macroify
/proc/sync_object(obj/game_object/obj, list/state)
	var/list/loc = state["loc"]
	if(loc == "Null")
		obj.loc = null
	else
		var/sloc = loc[1]
		switch(sloc)
			if("Within")
				obj.loc = GET_OBJECT(loc[sloc])
			if("Coords")
				var/list/c = loc[sloc]
				obj.loc = locate(c[1], c[2], c[3])

	var/list/appearance = state["appearance"]
	for(var/entry in appearance["entries"])
		sync_appearance(obj, entry)

//TODO: macroify
/proc/sync_appearance(obj/game_object/obj, list/appearance)
	// quickly map vars that can be set directly to json value
	#define M(v) \
		if(#v in appearance) {\
			obj.##v = appearance[#v];\
		}

	M(alpha)
	M(appearance_flags)
	M(blend_mode)
	M(desc)
	M(icon_state)
	M(infra_luminosity)
	M(invisibility)
	M(layer)
	M(luminosity)
	M(maptext)
	M(maptext_width)
	M(maptext_height)
	M(maptext_x)
	M(maptext_y)
	M(name)
	M(opacity)
	M(pixel_x)
	M(pixel_y)
	M(pixel_w)
	M(pixel_z)
	M(plane)
	M(transform)

	#undef M

	if("color" in appearance)
		var/color = appearance["color"]
		if(color == "White")
			obj.color = null
		else
			var/ck = color[1]
			var/list/cv = color[ck]
			switch(ck)
				if("RGB")
					obj.color = rgb(cv[1], cv[2], cv[3])
				if("Matrix")
					obj.color = cv
	if("gender" in appearance)
		var/gender = appearance["gender"]
		switch(gender)
			if("Neuter")
				obj.gender = NEUTER
			if("Male")
				obj.gender = MALE
			if("Female")
				obj.gender = FEMALE
			if("Plural")
				obj.gender = PLURAL
	if("icon" in appearance)
		obj.icon = iconmap[appearance["icon"]]
