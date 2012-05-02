var ProviderManager = jQuery.Class.create({
    init: function (baseUrl) {
        this.baseUrl = baseUrl;
    },

    getProvider: function (id, outputFormat, responseContainer) {
        responseContainer.val("");
        if(id == ""){
            alert("Please specify provider id");
            return;
        }
        var url = this.baseUrl + "/provider/" + encodeURIComponent(id) + "?view=" + outputFormat;

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

    createProvider: function (id, gatewayId, bulkGatewayId, isReceiptsSupported, outputFormat, responseContainer) {
        responseContainer.val("");
        if(gatewayId == ""){
            alert("Please specify gateway id");
            return;
        }
        if(id == ""){
            alert("Please specify provider id");
            return;
        }
        if(bulkGatewayId ==""){
            alert("Please specify bulk gateway id");
            return;
        }

        var url = this.baseUrl + "/provider?view=" + outputFormat;

        $.ajax({
            thisObj:this,
            type:"POST",
            data: ["id=",encodeURIComponent(id),"&gateway=", encodeURIComponent(gatewayId),"&bulk_gateway=",encodeURIComponent(bulkGatewayId),"&receipts_supported=",isReceiptsSupported].join(""),
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

    deleteProvider: function (id, outputFormat, responseContainer){
        responseContainer.val("");
        if(id == ""){
            alert("Please specify provider id");
            return;
        }

        var url = this.baseUrl + "/provider/" + encodeURIComponent(id)+ "?view=" + outputFormat;

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
