/* global HTMLWidgets, LeafletWidget, L, Shiny */

(function() {
  "use strict";

  function eventToShiny(event) {
    var shinyEvent = {
      latlng: {
        lat: event.latlng.lat,
        lng: event.latlng.lng
      }
    };

    if (event.title) {
      shinyEvent.title = event.title;
    }
    if (event.layer && event.layer.toGeoJSON) {
      shinyEvent.layer = event.layer.toGeoJSON();
    }

    return shinyEvent;
  }

  LeafletWidget.methods.addUnalRSearchFeatures = function(targetGroups, options) {
    var map = this;
    var searchFeatureGroup;

    if (map.searchControl) {
      map.searchControl.remove(map);
      delete map.searchControl;
    }

    options = options || {};
    options.marker = options.marker || {};

    if (options.moveToLocation) {
      options.moveToLocation = function(latlng, title, targetMap) {
        var zoom = options.zoom || 16;
        var maxZoom = targetMap.getMaxZoom();

        if (maxZoom && zoom > maxZoom) {
          zoom = maxZoom;
        }
        targetMap.setView(latlng, zoom);
      };
    }

    if (Array.isArray(targetGroups)) {
      searchFeatureGroup = map.layerManager.getLayerGroup("unalr-search", true);
      targetGroups.forEach(function(groupName) {
        var target = map.layerManager.getLayerGroup(groupName, false);
        if (target) {
          searchFeatureGroup.addLayer(target);
        } else if (window.console) {
          window.console.warn('Group with ID "' + groupName + '" not found, skipping');
        }
      });
    } else {
      searchFeatureGroup = map.layerManager.getLayerGroup(targetGroups, false);
      if (!searchFeatureGroup) {
        throw new Error('Group with ID "' + targetGroups + '" not found');
      }
    }

    options.marker.icon = undefined;
    L.stamp(searchFeatureGroup);
    options.layer = searchFeatureGroup;

    map.searchControl = new L.Control.Search(options);
    map.searchControl.addTo(map);

    map.searchControl.on("search:cancel", function(event) {
      if (event.target.options.hideMarkerOnCollapse && this._markerSearch) {
        event.target._map.removeLayer(this._markerSearch);
      }
    });

    map.searchControl.on("search:locationfound", function(event) {
      if (options.openPopup && event.layer) {
        if (event.layer._layers) {
          Object.values(event.layer._layers).some(function(layer) {
            if (layer._popup) {
              layer._popup.options.autoClose = false;
              layer.openPopup();
              return true;
            }
            return false;
          });
        } else if (event.layer._popup) {
          event.layer.openPopup();
        }
      }

      if (!HTMLWidgets.shinyMode) {
        return;
      }
      if (Shiny.setInputValue) {
        Shiny.setInputValue(
          map.id + "_search_location_found",
          eventToShiny(event),
          {priority: "event"}
        );
      } else {
        Shiny.onInputChange(
          map.id + "_search_location_found",
          eventToShiny(event)
        );
      }
    });
  };
})();
