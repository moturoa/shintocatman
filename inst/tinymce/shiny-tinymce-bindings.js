
/* 
Dit komt uit https://github.com/mul118/shinyMCE maar dat package werkt niet met
modules of in een modal
*/

//(function(){

var shinymceInputBinding = new Shiny.InputBinding();
$.extend(shinymceInputBinding, {
  find: function(scope) {
    return $(scope).find(".shinytinymce");
  },
  getValue: function(el) {
    return tinyMCE.get($(el).attr('id')).getContent();
  },
  setValue: function(el, value) {
    //tinyMCE.get($(el).attr('id')).change();
  },
  receiveMessage: function(el, value){
    this.setValue(el, value);
  },
  subscribe: function(el, callback) {  
    tinyMCE.get($(el).attr('id')).on("init", function(e) {
             callback(true);
     });  
    tinyMCE.get($(el).attr('id')).on("change", function(e) {
                 callback(true);
         });     
    //$(el).on('change.shinymceInputBinding', function(e){callback();});
  },
  unsubscribe: function(el) {
    $(el).off(".shinymceInputBinding");
  }
  //getRatePolicy: function(){
  //  return {
  //    policy: 'direct'
  //  }
  //}
});
Shiny.inputBindings.register(shinymceInputBinding);


Shiny.addCustomMessageHandler('shinyMCE.update', function(data) {
  tinyMCE.get(data.id).setContent(data.content);
  $('#'+data.id).trigger('change');
});


//})();

