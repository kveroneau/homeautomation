import xml.etree.ElementTree as ET
import urllib, datetime

class WeatherException(Exception):
    pass

class CalgaryWeather(object):
    """
    This class is used so that we can easily cache the data for multiple widgets to use.
    """
    calgary_url = 'http://dd.weather.gc.ca/citypage_weather/xml/AB/s0000047_e.xml'
    def __init__(self):
        self.weather_data = None
        self.last_update = None
    def get_calgary_weather(self):
        try:
            city = ET.fromstring(urllib.urlopen(self.calgary_url).read())
        except:
            raise WeatherException('Unable to obtain forcast.')
        try:
            current = city.findall('./currentConditions')[0]
        except:
            raise WeatherException('XML element parse error.')
        self.weather_data = {}
        self.weather_data['conditions'] = current.findall('./condition')[0].text
        self.weather_data['temperature'] = current.findall('./temperature')[0].text
        self.weather_data['feels_like'] = current.findall('./temperature')[0].text
        self.weather_data['futurecast'] = []
        for count in range(0, 5):
            forcast = {}
            day = city.findall('./forecastGroup/forecast')[count]
            forcast['title'] = day.findall('./period')[0].attrib['textForecastName']
            if day.findall('./temperatures/temperature')[0].attrib['class'] == 'high':
                forcast['temperature'] = day.findall('./temperatures/temperature')[0].text
            else:
                forcast['temperature'] = day.findall('./temperatures/temperature')[0].text
            forcast['outlook'] = day.findall('./textSummary')[0].text
            self.weather_data['futurecast'].append(forcast)
    @property
    def weather(self):
        if isinstance(self.last_update, datetime.datetime):
            dt = datetime.datetime.now()-self.last_update
            if dt.seconds > 3600:
                self.weather_data = None
        if self.weather_data is None:
            try:
                self.get_calgary_weather()
                self.last_update = datetime.datetime.now()
            except WeatherException, e:
                self.weather_data['error'] = str(e)
        return self.weather_data

weather_cache = CalgaryWeather()
