var GatewayManager = jQuery.Class.create({
    init: function (baseUrl) {
        this.baseUrl = baseUrl;
    },

    getGateway: function (id, outputFormat, responseContainer) {
        responseContainer.val("");
        if(id == ""){
            alert("Please specify gateway id");
            return;
        }
        var url = this.baseUrl + "/gateway/" + encodeURIComponent(id) + "?view=" + outputFormat;

        $.ajax({
            thisObj:this,
            type:"GET",
            dataType:"text",
            url:url,
            success:function(response){
                responseContainer.val(response);
            },
            error:function(xhr,error){
                alert("An error has occured during the request sending.");
            }
        });
    },

    createGateway: function (id, name, rps, outputFormat, responseContainer) {
        responseContainer.val("");
        if(id == ''){
            alert("Please specify gateway id");
            return;
        }
        if(name == ""){
            alert("Please specify gateway name");
            return;
        }
        if(rps == ''){
            alert("Please specify RPS parameter");
            return;
        }

        if(!IsInteger(rps)){
            alert("'rps' parameter should be integer");
            return;
        }

        var url = this.baseUrl + "/gateway?view=" + outputFormat;

        $.ajax({
            thisObj:this,
            type:"POST",
            data: ["id=",encodeURIComponent(id),"&name=", encodeURIComponent(name),"&rps=",encodeURIComponent(rps)].join(""),
            dataType: "text",
            url:url,
            success:function(response){
                responseContainer.val(response);
            },
            error:function(xhr,error){
                alert("An error has occured during the request sending.");
            }
        });
    },

    deleteGateway: function (id, outputFormat, responseContainer){
        responseContainer.val('');
        if(id == ''){
            alert("Please specify gateway id");
            return;
        }

        var url = this.baseUrl + "/gateway/" + encodeURIComponent(id)+ "?view=" + outputFormat;

        $.ajax({
            thisObj:this,
            type:"DELETE",
            dataType:"text",
            url:url,
            success:function(response){
                responseContainer.val(response);
            },
            error:function(xhr,error){
                alert("An error has occured during the request sending.");
            }
        });
    }
});
