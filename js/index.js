import 'ol/ol.css';
import Map from 'ol/Map';
import View from 'ol/View';
import GeoJSON from 'ol/format/GeoJSON';
import Cluster from 'ol/source/Cluster';
import {Tile as TileLayer, Vector as VectorLayer} from 'ol/layer';
import {OSM, TileWMS, Vector as VectorSource} from 'ol/source';
import {Circle as CircleStyle, Fill, Stroke, Style, Text} from 'ol/style';
import {createEmpty, getWidth, getHeight, extend} from 'ol/extent';

var dateSelect = new Date();
var intervalId = null;
var maxFeatureCount;
var vector = null;
var currentResolution = null;

// https://bulma.io/documentation/components/navbar/
document.addEventListener('DOMContentLoaded', () => {
    const $clickToggles = document.querySelectorAll('.click-toggle');

    if ($clickToggles.length > 0) {
	$clickToggles.forEach( el => {
	    el.addEventListener('click', () => {
		const target = el.dataset.target;
		const $target = document.getElementById(target);

		el.classList.toggle('is-active');
		$target.classList.toggle('is-active');
	    });
	});
    }

    document.getElementById('add-day').addEventListener('click', addDay);
    document.getElementById('sub-day').addEventListener('click', subDay);
    document.getElementById('date-text').addEventListener('click', playTimeline);
    updateStatusText();
});

function addToDay(val) {
    dateSelect.setDate(dateSelect.getDate() + val);
    updateStatusText();
    currentResolution = null;
    vector.getSource().refresh();
}
function addDay() { addToDay(1) };
function subDay() { addToDay(-1) };

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

function playTimeline() {
    if(intervalId !== null) {
	window.clearInterval(intervalId);
	intervalId = null;
	return;
    }
    var dateLimit = new Date();
    dateLimit.setDate(dateLimit.getDate() - 1);

    if(dateSelect >= dateLimit) dateSelect = new Date("2020-01-20");

    intervalId = window.setInterval(function () {
	addDay();
	if(dateSelect > dateLimit) {
	    window.clearInterval(intervalId);
	    intervalId = null;
	}
    }, 500);
}

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

function styleFunction(feature, resolution) {
    if (resolution != currentResolution) {
	calculateClusterInfo(resolution);
	currentResolution = resolution;
    }
    var style;
    var size = feature.get('features').length;

    return new Style({
	image: new CircleStyle({
            radius: Math.max(Math.min(30, 5*Math.log(size)), 10),
            fill: new Fill({
		color: [255, 13, 33, Math.min(0.8, 0.4 + (size / maxFeatureCount))]
            })
	}),
	text: new Text({
            text: size.toString(),
            fill: new Fill({
		color: '#fff'
	    }),
            stroke: new Stroke({
		color: 'rgba(0, 0, 0, 0.6)',
		width: 3
	    })
	})
    });
}

var vector = new VectorLayer({
    source: new Cluster({
	source: new VectorSource({
	    format: new GeoJSON(),
	    url: '/geo.json'
	}),
	geometryFunction: function(feature) {
	    if(new Date(feature.get('notification_date')) > dateSelect) {
		return null;
	    } else {
		return feature.getGeometry();
	    }
	},
	distance: 30
    }),
    style: styleFunction
});

var map = new Map({
    layers: [
	new TileLayer({
	    source: new OSM()
	}),
	new TileLayer({
//	    extent: [-13884991, 2870341, -7455066, 6338219],
	    source: new TileWMS({
		url: 'http://localhost:8080/geoserver/cite/wms',
		params: {'LAYERS': 'POA_2016_AUST', 'STYLES': 'red'},//, 'VERSION': '1.1.0', 'request': 'GetMap', 'srs': 'EPSG-4283'},
		serverType: 'geoserver',
		// Countries have transparency, so do not fade tiles:
	    })
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
