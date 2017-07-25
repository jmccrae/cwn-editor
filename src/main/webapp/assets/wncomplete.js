function wncomplete(id, context) {
  $(function() {
    function split( val ) {
      return val.split( /,\s*/ );
    }
    function extractLast( term ) {
      return split( term ).pop();
    }
 
    $( id )
      // don't navigate away from the field on tab when selecting an item
      .bind( 'keydown', function( event ) {
        if ( event.keyCode === $.ui.keyCode.TAB &&
            $( this ).autocomplete( 'instance' ).menu.active ) {
          event.preventDefault();
        }
      })
      .autocomplete({
        source: function( request, response ) {
          $.getJSON( context + '/wn/' + request.term, response );
        },
        search: function() {
          // custom minLength
          var term = extractLast( this.value );
          if ( term.length < 2 ) {
            return false;
          }
        },
        focus: function() {
          // prevent value inserted on focus
          return false;
        },
        select: function( event, ui ) {
          this.value = ui.item.value;
          return false;
        },
        position: { my: "left bottom", at: "left top", collision: "none" }
      });
  });
} 
