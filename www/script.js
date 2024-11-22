// Lat/lng bounds of the continental US
const BOUNDS = {
  south: 24.5, north: 49.0,
  west: -125.0, east: -66.9,
}

// callback for google location searchbox
function initAutocomplete() {
  const searchbox = document.getElementById('searchbox');
  const opts = {
    types: ['geocode'],
    bounds: BOUNDS,
    strictBounds: true,
  }
  autocomplete = new google.maps.places.Autocomplete(searchbox, opts);
  autocomplete.setFields(['name', 'geometry']);
  autocomplete.addListener('place_changed', function() {
    const place = autocomplete.getPlace();
    console.log(place)
    if (!place.geometry) return;
    const loc = place.geometry.location;
    const response = { name: place.name, lat: loc.lat(), lng: loc.lng() }
    Shiny.setInputValue('searched_loc', response, { priority: 'event' });
  });
}
