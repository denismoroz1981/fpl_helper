Shiny.addCustomMessageHandler("scrollCallback",
                             function(message){
                               
                             var scroll=setInterval(function(){window.scrollBy(0,500);},500);
                             setTimeout(function(){clearInterval(scroll);},1000);
                              
                              
                              }
                              
                              
                              
);  