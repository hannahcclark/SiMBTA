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

	$('#analyze').hide();
	// Populate Analysis Form
	for (var i in STATIONS) {
		var option = '<option value="'+STATIONS[i].slug+'">'
			+STATIONS[i].display+'</option>';
		$('#origin_select').append(option);
		$('#dest_select').append(option);
	}

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
		url: '/outFile.txt',
		success: function(text) {
			var data = parseData(text);
			drawFrame(1, data);
			$('#minutecount').text("Minute: 1 / "+(data.length-3));

			// Slider Event Listener
			$('#slider').on("slide", function(e, ui) {
				drawFrame(ui.value, data);
				$('#minutecount').text("Minute: "+ui.value+" / "
					+(data.length-3));
			})

			// Set Slider Options
			$("#slider").slider("option", "min", 1);
			$("#slider").slider("option", "max", data.length-3);

			// Button Event Listener
			$('#analyze-link').click(function() {
				$('#analyze').slideDown(250);
				return false;
			})

			// Combine All Passengers Data 
			// We Don't Need It on an Per-Minute Basis
			var passengersAggregate = [];
			for (var i in data) {
				if (data[i].passengers != []) {
					for (var j in data[i].passengers) {
						passengersAggregate.push(data[i].passengers[j])
					}
				}
			}

			// Average Calculator
			$('#calc_average').click(function() {

				var durations = [];
				var origin = $('#origin_select').val();
				var destination = $('#dest_select').val();

				// Find All Passenger Trips that Match the Start and End
				for (var i in passengersAggregate) {
					if (passengersAggregate[i].Start == origin && 
						passengersAggregate[i].End == destination) {
						durations.push(passengersAggregate[i].Duration);
					}
				}

				// Calculate Average and Return to User
				if (durations.length == 0) {
					$('#avg-result').html("No data available.");
				} else {
					var average = durations.reduce(function(a,b) { 
						return parseInt(a)+parseInt(b); 
					})/(durations.length);
					$('#avg-result').html("Average: <strong>"
						+Math.round(average)+"</strong> Minutes");
				}

				return false;

			})

		},
		error: function() {
			// Are You Running on a Web Server?
			alert('Could not open data file.');
		}
	})

});

var parseData = function(text) {

	var data = [];

	// Turns a Line of a File Into an Object
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

	// Go Through the File
	var currMinute = 0;
	var lines = text.split("\n");
	for (var i in lines) {
		var chunks = lines[i].split(" ");

		// Switch Based on First Word in the Line
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

// Canvas Drawing
var drawFrame = function(min, data) {

	var mapCanvas = document.getElementById("map");
	var mcx = mapCanvas.getContext("2d");

	mcx.clearRect(0, 0, mapCanvas.width, mapCanvas.height);

	var minuteData = data[min];
	// Turn List of Stations Into Associative Array by Station Slug
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
	// Each Station Has One Train Contained in it
	//   and an Array of Trains Waiting to Enter
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

	// Group All Train Data by Station
	for (var i in minuteData.trains) {
		var train = minuteData.trains[i];
		if (train.Approaching !== undefined && 
			train.Approaching != "endStation") {
			trainsByStation[train.Approaching].approaching[train.Direction]
																.push(train);
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

	// Draw all Trains Next to Stations
	for (var name in trainsByStation) {
		
		if (trainsByStation[name].approaching.alewife.length != 0) {
			trainsByStation[name].approaching.alewife.forEach(function(el, i) {
				drawStation(200 + TRAINOFFSET + (i*20), 
					trainsByStation[name].station.y+20, el.Passengers);
			})
		}

		if (trainsByStation[name].approaching.ashmont.length != 0) {
			trainsByStation[name].approaching.ashmont.forEach(function(el, i) {
				drawStation(200 - TRAINOFFSET - (i*20), 
					trainsByStation[name].station.y-20, el.Passengers);
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

	// Anti-Aliasing Hack
	//mcx.translate(0.5, 0.5);

}