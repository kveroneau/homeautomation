all: LightController LightApp HTTPEndpoints

LightController: LightController.lpr
	lazbuild --bm=Release LightController.lpr

LightApp: LightApp.lpr
	lazbuild --bm=Release LightApp.lpr

HTTPEndpoints: HTTPEndpoints.lpr
	lazbuild --bm=Release HTTPEndpoints.lpr
	cp lib/x86_64-linux/HTTPEndpoints ./

clean:
	rm -rf lib LightController LightApp HTTPEndpoints LightApp.res
