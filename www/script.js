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
    console.log(place)
    if (!place.geometry) return;
    const loc = place.geometry.location;
    const response = { name: place.name, lat: loc.lat(), lng: loc.lng() }
    sendShiny('searched_loc', response);
  });
}

// get locality name from coordinates
function getLocalityName(lat, lng, apiKey) {
  const url = `https://maps.googleapis.com/maps/api/geocode/json?latlng=${lat},${lng}&key=${apiKey}`;
  const inputId = 'locality_name'
  const loc = { lat: lat, lng: lng, name: 'Clicked point' }

  fetch(url)
    .then(response => response.json())
    .then(data => {
      const addressComponents = data.results[0].address_components;
      const locality = addressComponents.find(component => 
        component.types.includes('locality')
      );
      const city = locality ? locality.long_name : fallbackName;
      console.log('Nearest city:', city);
      loc.name = city
      sendShiny(inputId, loc)
    })
    .catch(error => {
      console.error('Error:', error)
      sendShiny(inputId, loc)
    });
}


//--- Cookie handling ---//

const COOKIE_NAME = 'ibm_weather_tool'

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
  return name;
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
  sendShiny('cookie', value);
  return value;
}

function deleteCookie(name = COOKIE_NAME) {
  document.cookie = name + '=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;';
  return name;
}


//--- Site action buttons ---//

function sendShiny(inputId, content) {
  Shiny.setInputValue(inputId, content, { priority: 'event'} )
}

function editSite(site_id, site_name = '') {
  let newName = prompt(`Enter a new name for site ${site_id}:`, site_name);
  if (newName) sendShiny('edit_site', { id: site_id, name: newName } );
}

function trashSite(site_id) {
  if (confirm(`Remove site ${site_id}?`)) sendShiny('trash_site', site_id);
}

function saveSite(site_id) {
  sendShiny('save_site', site_id);
}

function clearSites() {
  if (confirm('Remove all sites?')) sendShiny('clear_sites', true);
}
