var STATIONS = [
	{
		"slug": "alewife",
		"display": "Alewife"
	}, {
		"slug": "davis",
		"display": "Davis"
	}, {
		"slug": "porter",
		"display": "Porter"
	}, {
		"slug": "harvard",
		"display": "Harvard"
	}, {
		"slug": "central",
		"display": "Central"
	}, {
		"slug": "kendall",
		"display": "Kendall"
	}, {
		"slug": "charles_mgh",
		"display": "Charles/MGH"
	}, {
		"slug": "park_street",
		"display": "Park Street"
	}, {
		"slug": "downtown_crossing",
		"display": "Downtown Crossing"
	}, {
		"slug": "south",
		"display": "South Station"
	}, {
		"slug": "broadway",
		"display": "Broadway"
	}, {
		"slug": "andrew",
		"display": "Andrew"
	}, {
		"slug": "jfk_umass",
		"display": "JFK/UMass"
	}, {
		"slug": "savin_hill",
		"display": "Savin Hill"
	}, {
		"slug": "fields_corner",
		"display": "Fields Corner"
	}, {
		"slug": "shawmut",
		"display": "Shawmut"
	}, {
		"slug": "ashmont",
		"display": "Ashmont"
	}
];

var OFFSET = 40;
var TRAINOFFSET = 25;

$(document).ready(function() {

	// Initialize the Slider
	$('#slider').slider({
		value: 0,
		min:0,
		max:1,
		step:1
	})

	// Load Data
	$.ajax({
		type: 'GET',
		url: 'output.txt',
		success: function(text) {
			var data = parseData(text);
			drawFrame(1, data);

			$('#slider').on("slide", function(e, ui) {
				drawFrame(ui.value, data);				
			})

			$("#slider").slider("option", "max", data.length-1);

		},
		error: function() {
			alert('Could not open data file.');
		}
	})

});

var parseData = function(text) {

	var data = [];

	var parseLine = function(chunks) {
		var obj = {};
		for (var i = 1; i < chunks.length; i++) {
			var chk = chunks[i].split(":");
			obj[chk[0]] = chk[1];
		}
		return obj;
	}

	// Fix a Pesky Whitespace Bug
	text = text.replace(new RegExp(": ", "g"), ":")
	console.log(text);

	var currMinute = 0;
	var lines = text.split("\n");
	for (var i in lines) {
		var chunks = lines[i].split(" ");

		switch(chunks[0]) {
			case "Minute":
				currMinute = parseInt(chunks[1]);
				data[currMinute] = {
					trains: [],
					stations: [],
					passengers: []
				};
				break;

			case "train":
				data[currMinute].trains.push(parseLine(chunks));
				break;

			case "station":
				data[currMinute].stations.push(parseLine(chunks));
				break;

			case "passenger":
				data[currMinute].passengers.push(parseLine(chunks));
				break;

		}

	}

	return data;

}

var drawFrame = function(min, data) {

	console.log("DRAWING "+min);

	var mapCanvas = document.getElementById("map");
	var mcx = mapCanvas.getContext("2d");

	mcx.clearRect(0, 0, mapCanvas.width, mapCanvas.height);

	var minuteData = data[min];
	console.log(minuteData);
	var dataByStation = (function() {
		var data = {};
		for (var i in minuteData.stations) {
			data[minuteData.stations[i].Name] = minuteData.stations[i];
		}
		return data;
	})();

	// Draw the Red Line
	mcx.lineWidth = 15;
	mcx.strokeStyle = '#ff0000';
	mcx.beginPath();
	mcx.moveTo(200, 40);
	mcx.lineTo(200, STATIONS.length*40);
	mcx.stroke();

	for (var i in STATIONS) {

		var s = STATIONS[i];
		s.y = (parseInt(i)+1)*40;

		// Draw Station Dot
		mcx.beginPath();
		if (s.slug == "ashmont" || s.slug == "alewife") {
			mcx.arc(200, s.y, 10, 0, 2 * Math.PI, false);
     		mcx.fillStyle = "white";
     		mcx.fill();
     		mcx.lineWidth = 5;
     		mcx.strokeStyle = '#ff0000';
     		mcx.stroke();
		} else {
			mcx.arc(200, s.y, 6, 0, 2 * Math.PI, false);
     		mcx.fillStyle = "white";
     		mcx.fill();
		}
     	mcx.closePath();

		// Draw Station Text
		mcx.textAlign = "left";
		mcx.textBaseline = "middle";
		mcx.fillStyle = "black";
		if (s.slug == "ashmont" || s.slug == "alewife") {
			mcx.font = "bold 14pt Arial";
			mcx.fillText(s.display.toUpperCase(), 200+OFFSET, s.y);
		} else {
			mcx.font = "10pt Arial";
			mcx.fillText(s.display, 200+OFFSET, s.y);
		}
		

		// Draw Passenger Count
		mcx.textAlign = "right";
		mcx.textBaseline = "middle";
		mcx.fillStyle = "black";
		mcx.font = "8pt Arial";
		if (dataByStation[s.slug] !== undefined) {
			mcx.fillText(dataByStation[s.slug].Passengers, 200-OFFSET, s.y);
		} else {
			mcx.fillText("0", 200-OFFSET, s.y);
		}

	}

	// Draw Active Trains
	var getYOfStation = function(stationName) {
		for (var i in STATIONS) {
			if (STATIONS[i].slug == stationName) {
				return STATIONS[i].y;
			}
		}
		return 0;
	}
	// Create Initial Array of Empty Values for Each Station
	var trainsByStation = STATIONS.reduce(function(obj, station, i) {
		obj[station.slug] = {
			station: station,
			approaching: {
				alewife: [],
				ashmont: []
			},
			contained: {
				alewife: null,
				ashmont: null
			}
		}
		return obj;
	}, {});
	for (var i in minuteData.trains) {
		var train = minuteData.trains[i];
		console.log(train);
		if (train.Approaching !== undefined) {
			trainsByStation[train.Approaching].approaching[train.Direction].push(train);
		} else if (train.Station !== undefined) {
			trainsByStation[train.Station].contained[train.Direction] = train;
		}
	};

	var drawStation = function(x, y, numPassengers) {
		mcx.fillStyle = "#c0c0c0";
		mcx.beginPath();
		mcx.arc(x, y, 10, 0, 2 * Math.PI, false);
		mcx.fill();
     	mcx.closePath();

     	mcx.fillStyle = "black";
     	mcx.font = "7pt Arial";
     	mcx.textAlign = "center";
		mcx.textBaseline = "middle";
     	mcx.fillText(numPassengers, x, y);
	}

	for (var name in trainsByStation) {
		
		if (trainsByStation[name].approaching.alewife.length != 0) {
			trainsByStation[name].approaching.alewife.forEach(function(el, i) {
				drawStation(200 + TRAINOFFSET + (i*20), trainsByStation[name].station.y+20, el.Passengers);
			})
		}

		if (trainsByStation[name].approaching.ashmont.length != 0) {
			trainsByStation[name].approaching.ashmont.forEach(function(el, i) {
				drawStation(200 - TRAINOFFSET - (i*20), trainsByStation[name].station.y-20, el.Passengers);
			})
		}

		if (trainsByStation[name].contained.ashmont != null) {
			drawStation(200 - TRAINOFFSET, trainsByStation[name].station.y, 
				trainsByStation[name].contained.ashmont.Passengers);
		}

		if (trainsByStation[name].contained.alewife != null) {
			drawStation(200 + TRAINOFFSET, trainsByStation[name].station.y,
				trainsByStation[name].contained.alewife.Passengers);
		}
		
	};

	// Draw Active Passengers

	// Anti-Aliasing Hack
	//mcx.translate(0.5, 0.5);

}