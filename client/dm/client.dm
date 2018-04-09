#define LIB_PATH "../target/debug/mclient.dll"

/world/New()
	for(var/i = 0; i < 10; i++)
		world.log << json_encode(call(LIB_PATH, "hello")("[i]"))
	del(src)
