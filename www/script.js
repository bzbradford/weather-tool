// JS functions for Researcher's Weather Tool

//--- Google Places integration ---//

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
    // console.log(place)
    if (!place.geometry) return;
    const loc = place.geometry.location;
    const response = { name: place.name, lat: loc.lat(), lng: loc.lng() }
    Shiny.setInputValue('searched_loc', response, { priority: 'event' });
  });
}


//--- Cookie handling ---//

COOKIE_NAME = 'ibm_weather_tool_sites'

function setCookie(value, name = COOKIE_NAME, days = 30) {
  let expires = '';
  if (days) {
    let date = new Date();
    date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
    expires = '; expires=' + date.toUTCString();
  }
  if (typeof value === 'object') {
    value = JSON.stringify(value);
  }
  document.cookie = name + '=' + value + expires + '; path=/';
  return(name)
}

function getCookie(name = COOKIE_NAME) {
  let nameEQ = name + '=';
  let ca = document.cookie.split(';');
  for(let i = 0; i < ca.length; i++) {
    let c = ca[i];
    while (c.charAt(0) == ' ') c = c.substring(1, c.length);
    if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length, c.length);
  }
  return null;
}

function sendCookieToShiny(name = COOKIE_NAME) {
  let value = getCookie(name);
  Shiny.setInputValue('cookie', value);
  return(value)
}

function deleteCookie(name = COOKIE_NAME) {
  document.cookie = name + '=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;';
  return(name)
}
