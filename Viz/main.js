var STATIONS = [
	{
		"slug": "alewife",
		"display": "Alewife"
	},
	{
		"slug": "davis",
		"display": "Davis"
	},
	{
		"slug": "porter",
		"display": "Porter"
	},
	{
		"slug": "harvard",
		"display": "Harvard"
	},
	{
		"slug": "central",
		"display": "Central"
	},
	{
		"slug": "kendall",
		"display": "Kendall"
	},
	{
		"slug": "charles_mgh",
		"display": "Charles/MGH"
	},
	{
		"slug": "park_street",
		"display": "Park Street"
	},
	{
		"slug": "downtown_crossing",
		"display": "Downtown Crossing"
	},
	{
		"slug": "south",
		"display": "South Station"
	},
	{
		"slug": "broadway",
		"display": "Broadway"
	},
	{
		"slug": "andrew",
		"display": "Andrew"
	},
	{
		"slug": "jfk_umass",
		"display": "JFK/UMass"
	},
	{
		"slug": "savin_hill",
		"display": "Savin Hill"
	},
	{
		"slug": "fields_corner",
		"display": "Fields Corner"
	},
	{
		"slug": "shawmut",
		"display": "Shawmut"
	},
	{
		"slug": "ashmont",
		"display": "Ashmont"
	}
];

$(document).ready(function() {

	// Load Data
	$.ajax({
		type: 'GET',
		url: 'output.txt',
		success: function(text) {
			var data = parseData(text);
			drawFrame(0, data);


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

	var mapCanvas = document.getElementById("map");
	var mcx = mapCanvas.getContext("2d");

	var minuteData = data[min];

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
		if (s.slug == "ashmont" || s.slug == "alewife") {
			mcx.fillStyle = "black";
			mcx.font = "bold 14pt Arial";
			mcx.fillText(s.display.toUpperCase(), 250, s.y+7);
		} else {
			mcx.fillStyle = "black";
			mcx.font = "10pt Arial";
			mcx.fillText(s.display, 250, s.y+5);	
		}
		

		// Draw Passenger Count

	}

	// Draw Active Trains

	// Draw Active Passengers

}