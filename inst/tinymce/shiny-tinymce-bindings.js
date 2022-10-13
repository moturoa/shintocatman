
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
    tinyMCE.get($(el).attr('id')).change(value);
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
     tinyMCE.get($(el).attr('id')).on("input", function(e) {
                 callback(true);
         });     
    
    
    
  },
  unsubscribe: function(el) {
    $(el).off(".shinymceInputBinding");
  }
  
});
Shiny.inputBindings.register(shinymceInputBinding);


Shiny.addCustomMessageHandler('shinyMCE.update', function(data) {
  
  let editor = tinyMCE.get(data.id); 
  
  editor.setContent(data.content);
  editor.fire("input");
  
});

