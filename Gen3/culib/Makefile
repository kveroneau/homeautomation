all: LightController LightApp HTTPEndpoints Automation

LightController: LightController.lpr
	lazbuild --bm=Release LightController.lpr

LightApp: LightApp.lpr
	lazbuild --bm=Release LightApp.lpr

HTTPEndpoints: HTTPEndpoints.lpr
	lazbuild --bm=Release HTTPEndpoints.lpr
	cp lib/x86_64-linux/HTTPEndpoints ./

Automation: Automation.lpr
	lazbuild --bm=Release Automation.lpr

clean:
	rm -rf lib LightController LightApp HTTPEndpoints Automation LightApp.res
