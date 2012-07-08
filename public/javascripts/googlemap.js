 function initGoogleMap(element_id){
        address = $('#'+element_id).attr('text');
        image = $('#'+element_id).attr('image');

        if (address != "None"){

        var styleArray = [
        {
          featureType: "all",
          stylers: [
            { saturation: -90 }
          ]
          }];

        var myOptions = {
          zoom: 12,
          disableDefaultUI: true,
          mapTypeId: google.maps.MapTypeId.ROADMAP,
          styles: styleArray
        };

        var geocoder = new google.maps.Geocoder();

        geocoder.geocode( { 'address': address}, function(results, status) {
          if (status == google.maps.GeocoderStatus.OK) {
              var map = new google.maps.Map(document.getElementById(element_id),
              myOptions);

            map.setCenter(results[0].geometry.location);
            map.fitBounds(results[0].geometry.bounds);
           var marker = new google.maps.Marker({
              position: results[0].geometry.location,
              map: map,
              title:address,
              icon:image
            });

          } else {
            $('#'+element_id).hide();
            console.log("Geocode was not successful for the following reason: " + status);
          }
        });
        }else{
          $('#'+element_id).hide();
          $('.loc-head').hide();
        }
         
    }
