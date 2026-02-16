package main

import (
	"encoding/json"
	"fmt"
	"io"
	"math"
	"net/http"
	"os"
	"strconv"
	"strings"
	"time"
)

func exitIfError(err error) {
	if err != nil {
		fmt.Printf("%v\n", err)
		os.Exit(1)
	}
}

func must[T any](x T, err error) T {
	if err != nil {
		panic(err)
	}
	return x
}

func getJSON[T any](url string, t *T) {
	resp, err := http.Get(url)
	exitIfError(err)

	bytes, err := io.ReadAll(resp.Body)
	exitIfError(err)

	err = json.Unmarshal(bytes, t)
	exitIfError(err)
}

type ipinfo struct {
	latitude, longitude float64
}

func (info *ipinfo) UnmarshalJSON(text []byte) error {
	var tmp struct {
		Loc string `json:"loc"`
	}
	err := json.Unmarshal(text, &tmp)
	if err != nil {
		return err
	}
	latLong := strings.Split(tmp.Loc, ",")
	*info = ipinfo{
		latitude: must(strconv.ParseFloat(latLong[0], 64)),
		longitude: must(strconv.ParseFloat(latLong[1], 64)),
	}
	return nil
}

type Time struct {
	time.Time
}

func (t *Time) UnmarshalJSON(text []byte) error {
	text0 := strings.Trim(string(text), "\"")
	for _, s := range []string{"2006-01-02", "2006-01-02T15:04"} {
		t0, err := time.Parse(s, text0)
		if err == nil {
			*t = Time{t0}
			return nil
		}
	}
	return fmt.Errorf("Could not parse time: %s", string(text))
}

type current struct {
	Time *Time `json:"time"`
	Temperature2m float64 `json:"temperature_2m"`
	ApparentTemperature float64 `json:"apparent_temperature"`
	WeatherCode int `json:"weather_code"`
	RelativeHumidity int `json:"relative_humidity_2m"`
}

type daily struct {
	Time []*Time `json:"time"`
	Temperature2mMax []float64 `json:"temperature_2m_max"`
	Temperature2mMin []float64 `json:"temperature_2m_min"`
	Sunrise []*Time `json:"sunrise"`
	Sunset []*Time `json:"sunset"`
}

type hourly struct {
	Time []*Time `json:"time"`
	Temperature2m []float64 `json:"temperature_2m"`
	WeatherCode []int `json:"weather_code"`
}

type weather struct {
	Current current `json:"current"`
	Daily daily `json:"daily"`
	Hourly hourly `json:"hourly"`
}

func printSimple(w *weather) {
	fmt.Printf(
		"%s %s %d° (%d°)\n",
		weatherCodes[w.Current.WeatherCode].symbol,
		weatherCodes[w.Current.WeatherCode].description,
		int(math.Round(w.Current.Temperature2m)),
		int(math.Round(w.Current.ApparentTemperature)),
	)
}

func printDetailed(w *weather) {
	fmt.Printf(
		"%s %d°\nFeels like: %d°\nHumidity: %d%%\n\n",
		weatherCodes[w.Current.WeatherCode].description,
		int(math.Round(w.Current.Temperature2m)),
		int(math.Round(w.Current.ApparentTemperature)),
		w.Current.RelativeHumidity,
	)

	for i := range w.Daily.Time {
		time := w.Daily.Time[i]
		tempMax := w.Daily.Temperature2mMax[i]
		tempMin := w.Daily.Temperature2mMin[i]
		sunrise := w.Daily.Sunrise[i]
		sunset := w.Daily.Sunset[i]
		date := time.Format("2006-01-02")
		switch i {
			case 0: date = "Today, " + date
			case 1: date = "Tomorrow, " + date
		}
		fmt.Println(date)

		fmt.Printf(
			"⬆️ %d° ⬇️ %d° 🌅 %s 🌇 %s\n",
			int(math.Round(tempMax)),
			int(math.Round(tempMin)),
			sunrise.Format("15:04"),
			sunset.Format("15:04"),
		)

		for j := 24 * i; j < 24 * i + 24; j += 3 {
			if j % 3 == 0 {
				time := w.Hourly.Time[j]
				symbol := weatherCodes[w.Hourly.WeatherCode[j]].symbol
				temp := int(math.Round(w.Hourly.Temperature2m[j]))
				desc := weatherCodes[w.Hourly.WeatherCode[j]].description
				fmt.Printf(
					"%02d %s %4d° %s\n",
					time.Hour(),
					symbol,
					temp,
					desc,
				)
			}
		}

		fmt.Println()
	}
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "Usage: %s [simple|detailed]\n", os.Args[0])
		os.Exit(1)
	}

	if os.Args[1] != "simple" && os.Args[1] != "detailed" {
		fmt.Fprintf(os.Stderr, "Usage: %s [simple|detailed]\n", os.Args[0])
		os.Exit(1)
	}

	var ipinfo ipinfo

	getJSON("https://ipinfo.io", &ipinfo)

	baseURL := "https://api.open-meteo.com/v1/forecast?temperature_unit=fahrenheit&timezone=auto&forecast_days=3"
	location := fmt.Sprintf("latitude=%f&longitude=%f", ipinfo.latitude, ipinfo.longitude)
	current := "current=temperature_2m,apparent_temperature,weather_code,relative_humidity_2m"
	weatherURL := fmt.Sprintf(
		"%s&%s&%s",
		baseURL,
		location,
		current,
	)

	if os.Args[1] == "detailed" {
		weatherURL = weatherURL + "&daily=temperature_2m_max,temperature_2m_min,sunrise,sunset"
		weatherURL = weatherURL + "&hourly=temperature_2m,weather_code"
	}

	resp, err := http.Get(weatherURL)
	exitIfError(err)
	defer resp.Body.Close()

	bytes, err := io.ReadAll(resp.Body)
	exitIfError(err)

	var weather weather

	err = json.Unmarshal(bytes, &weather)
	exitIfError(err)

	if os.Args[1] == "simple" {
		printSimple(&weather)
	} else {
		printDetailed(&weather)
	}
}
