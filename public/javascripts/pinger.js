(function($){

  var Pinger = Backbone.Model.extend({
      defaults: {
                },
      initialize: function() { 
                  },
    });
  
  var ListView = Backbone.View.extend({    
    el: $('wtfdoweattachthisto'), // attaches `this.el` to an existing element.
    initialize: function(){

      _.bindAll(this, 'render'); // fixes loss of context for 'this' within methods
       
      this.model.view = this;
      this.model.bind("updated", this.render);
      
       this.render();
    },
    render: function(){
    }
  });

  var aPinger = new Pinger();
  var listView = new ListView({model: aPinger});      
})(jQuery);

