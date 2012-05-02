var CustomerUserManager = jQuery.Class.create({
    init: function (baseUrl) {
        this.baseUrl = baseUrl;
    },

    createCustomerUser: function (customerId,id,pass,permittedTypes,outputFormat,responseContainer) {
        responseContainer.val("");

        if (customerId == '') {
            alert("Please specify customer id");
            return;
        }
        if(id == ''){
            alert("Please specify customer user id");
            return;
        }
        if(pass == ''){
            alert("Please specify password");
            return;
          }
        if(permittedTypes == ''){
          alert("Please specify permitted SMPP types separated by a comma (transmitter,receiver,transceiver)");
            return;
        }

        var permittedTypesArray = permittedTypes.split(",");
        for(var i=0; i< permittedTypesArray.length; i++)
        {
            var type = $.trim(permittedTypesArray[i]);
            if(type != 'transmitter' && type != 'receiver' && type != 'transceiver'){
                alert("'Permitted SMPP types' field must be filled in with a comma-separated type values (transmitter,receiver,transceiver)");
                return;
            }
        }

        var url = this.baseUrl+"/customer/"+encodeURIComponent(customerId)+"/user?view="+outputFormat;

        $.ajax({
            thisObj:this,
            type: "POST",
            data: ["id=", id, "&pswd=", pass, "&permitted_smpp_types=", encodeURIComponent(permittedTypesArray.join(","))].join(""),
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

    deleteCustomerUser: function (customerId, id, outputFormat, responseContainer){
        responseContainer.val('');
        if(customerId == ''){
            alert("Please specify customer id");
            return;
        }
        if(id == ''){
            alert("Please specify customer user id");
            return;
        }

        var url = this.baseUrl+"/customer/"+encodeURIComponent(customerId)+"/user/"+encodeURIComponent(id)+"?view="+outputFormat;

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
