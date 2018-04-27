#define LIB_PATH "../target/debug/mclient.dll"

/world/New()
	call(LIB_PATH, "dibby_setup")()
	while(TRUE)
		var/ret = call(LIB_PATH, "dibby_recv")()
		world.log << json_encode(ret)
		if(ret == "")
			break
		sleep(10)
	call(LIB_PATH, "dibby_shutdown")()
	del(src)
