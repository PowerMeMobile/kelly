var NetworkManager = jQuery.Class.create({
    init: function (baseUrl) {
        this.baseUrl = baseUrl;
    },

    getNetwork: function (id, outputFormat, responseContainer) {
        responseContainer.val("");
        if(id == ""){
            alert("Please specify network id");
            return;
        }
        var url = this.baseUrl + "/network/" + encodeURIComponent(id) + "?view=" + outputFormat;

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

    createNetwork: function (id,countryCode,numbersLen,prefixes,providerId,outputFormat,responseContainer) {
        responseContainer.val("");
        if(id == ""){
            alert("Please specify network id");
            return;
        }
        if(countryCode == ""){
            alert("Please specify country code");
            return;
        }
        if(numbersLen == ""){
            alert("Please specify phone numbers length");
            return;
        }
        if(prefixes == ""){
            alert("Please specify prefixes");
            return;
        }
        if(providerId == ""){
            alert("Please specify provider id");
            return;
        }
        if(!IsInteger(countryCode)){
            alert("Country code should be integer");
            return;
        }
        if(!IsInteger(numbersLen)){
            alert("Phone numbers length should be integer");
            return;
        }
        var prefixesArray = prefixes.split(",");
        for(var i=0; i< prefixesArray.length; i++)
        {
            if(!IsInteger(prefixesArray[i])){
                alert("'Prefixes' field must be filled in with a comma-separated integer numbers");
                return;
            }
            prefixesArray[i]=$.trim(prefixesArray[i]);
        }
        var url = this.baseUrl + "/network?view=" + outputFormat;

        $.ajax({
            thisObj:this,
            type:"POST",
            data: ["id=",encodeURIComponent(id),"&country_code=",countryCode,"&numbers_len=",numbersLen,"&prefixes=",prefixesArray.join(","),"&provider_id=",encodeURIComponent(providerId)].join(""),
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

    deleteNetwork: function (id, outputFormat, responseContainer){
        responseContainer.val("");
        if(id == ""){
            alert("Please specify network id");
            return;
        }

        var url = this.baseUrl + "/network/" + encodeURIComponent(id)+ "?view=" + outputFormat;

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

