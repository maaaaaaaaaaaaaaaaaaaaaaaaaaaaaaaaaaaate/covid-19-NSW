
// https://bulma.io/documentation/components/navbar/
document.addEventListener('DOMContentLoaded', () => {
    const $navbarBurgers = Array.prototype.slice.call(document.querySelectorAll('.navbar-burger'), 0);

    if ($navbarBurgers.length > 0) {
	$navbarBurgers.forEach( el => {
	    el.addEventListener('click', () => {
		const target = el.dataset.target;
		const $target = document.getElementById(target);

		el.classList.toggle('is-active');
		$target.classList.toggle('is-active');
	    });
	});
    }
});

var dateSelect = new Date();

function addDay() {
    dateSelect.setDate(dateSelect.getDate() + 1);
    updateStatusText();
    vector.changed();
}
function subDay() {
    dateSelect.setDate(dateSelect.getDate() - 1);
    updateStatusText();
    vector.changed();
}

// https://stackoverflow.com/questions/23593052/format-javascript-date-as-yyyy-mm-dd
function formatDate(date) {
    var d = new Date(date),
        month = '' + (d.getMonth() + 1),
        day = '' + d.getDate(),
        year = d.getFullYear();

    if (month.length < 2)
        month = '0' + month;
    if (day.length < 2)
        day = '0' + day;

    return [year, month, day].join('-');
}

function updateStatusText() {
    document.getElementById('date-text').textContent = formatDate(dateSelect);
}

var intervalid = null;

function playTimeline() {
    if(intervalid !== null) {
	window.clearInterval(intervalid);
	intervalid = null;
	return;
    }
    var dateLimit = new Date();
    dateLimit.setDate(dateLimit.getDate() - 1);

    if(dateSelect >= dateLimit) dateSelect = new Date("2020-01-20");

    intervalid = window.setInterval(function () {
	addDay();
	if(dateSelect > dateLimit) {
	    window.clearInterval(intervalid);
	    intervalid = null;
	}
    }, 500);
}

document.getElementById('add-day').addEventListener('click', addDay);
document.getElementById('sub-day').addEventListener('click', subDay);
document.getElementById('date-text').addEventListener('click', playTimeline);
updateStatusText();


var textFill = new Fill({
    color: '#fff'
});
var textStroke = new Stroke({
    color: 'rgba(0, 0, 0, 0.6)',
    width: 3
});

var maxFeatureCount;
var vector = null;
var calculateClusterInfo = function(resolution) {
    maxFeatureCount = 0;
    var features = vector.getSource().getFeatures();
    var feature, radius;
    for (var i = features.length - 1; i >= 0; --i) {
	feature = features[i];
	var originalFeatures = feature.get('features');
	var extent = createEmpty();
	var j = (void 0), jj = (void 0);
	for (j = 0, jj = originalFeatures.length; j < jj; ++j) {
	    extend(extent, originalFeatures[j].getGeometry().getExtent());
	}
	maxFeatureCount = Math.max(maxFeatureCount, jj);
	radius = 0.25 * (getWidth(extent) + getHeight(extent)) /
            resolution;
	feature.set('radius', radius);
    }
};

var currentResolution;
function styleFunction(feature, resolution) {
    if (resolution != currentResolution) {
	calculateClusterInfo(resolution);
	currentResolution = resolution;
    }
    var style;
    var size = feature.get('features').length;
    feature.get('features').forEach(function (featureL1) {
	if(new Date(featureL1.get('notification_date')) > dateSelect) {
	    size--;
	}
    });
    if(size > 0) {
	style = new Style({
	    image: new CircleStyle({
		radius: feature.get('radius'),
		fill: new Fill({
		    color: [73, 153, 255, Math.min(0.8, 0.4 + (size / maxFeatureCount))]
		})
	    }),
	    text: new Text({
		text: size.toString(),
		fill: textFill,
		stroke: textStroke
	    })
	});
    }
    return style;
}

var vector = new VectorLayer({
    source: new Cluster({
	source: new VectorSource({
	    format: new GeoJSON(),
	    url: '/geo.json'
	}),
	distance: 30
    }),
    style: styleFunction
});

var map = new Map({
    layers: [
	new TileLayer({
	    source: new OSM()
	}),
	vector
    ],
    target: 'map',
    view: new View({
	projection: 'EPSG:4326',
	center: [150.916, -31.08],
	zoom: 5
    })
});
