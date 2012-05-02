var ConnectionManager = jQuery.Class.create({
    init: function (baseUrl) {
        this.baseUrl = baseUrl;
    },

    createConnection: function (gatewayId,id,type,ip,port,sysId,pass,sysType,addrTON,addrNPI,addrRange,outputFormat,responseContainer) {
        responseContainer.val("");

        if(gatewayId == ''){
            alert("Please specify gateway id");
            return;
        }
        if(id == ''){
            alert("Please specify connection id");
            return;
        }
        if(type == ''){
            alert("Please specify connection type");
            return;
        }
        if(ip == ''){
            alert("Please specify connection IP");
            return;
        }
        if(port == ''){
            alert("Please specify connection port");
            return;
        }
        if(sysId == ''){
            alert("Please specify system id");
            return;
        }
        if(pass == ''){
            alert("Please specify the password");
            return;
        }
        if(sysType == ''){
            alert("Please specify system type");
            return;
        }
        if(addrTON == ''){
            alert("Please specify address TON");
            return;
        }
        if(addrNPI == ''){
            alert("Please specify address NPI");
            return;
        }
        /*if(addrRange == ''){
            alert("Please specify address range");
            return;
        }*/
        if(!IsInteger(id)){
            alert("Connection id should be integer");
            return;
        }
        if(!IsInteger(type)){
            alert("Connection type should be integer");
            return;
        }
        if(!IsIPAddressValid(ip)){
            alert("Connection IP is not valid");
            return;
        }
        if(!IsInteger(port)){
            alert("Connection port should be integer");
            return;
        }
        if(!IsInteger(addrTON)){
            alert("Address TON should be integer");
            return;
        }
        if(!IsInteger(addrNPI)){
            alert("Address NPI should be integer");
            return;
        }

        var url = this.baseUrl+"/gateway/"+encodeURIComponent(gatewayId)+"/connection?view="+outputFormat;

        $.ajax({
            thisObj:this,
            type:"POST",
            data: ["id=",id,"&type=",type,"&addr=",ip,"&port=",port,"&sys_id=",encodeURIComponent(sysId),"&pass=",pass,"&sys_type=",sysType,"&addr_ton=",addrTON,"&addr_npi=",addrNPI,"&addr_range=",encodeURIComponent(addrRange)].join(""),
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

    deleteConnection: function (gatewayId, id, outputFormat, responseContainer){
        responseContainer.val('');
        if(gatewayId == ''){
            alert("Please specify gateway id");
            return;
        }
        if(id == ''){
            alert("Please specify connection id");
            return;
        }
        if(!IsInteger(id)){
            alert("Connection id should be integer");
            return;
        }

        var url = this.baseUrl+"/gateway/"+encodeURIComponent(gatewayId)+"/connection/"+encodeURIComponent(id)+"?view="+outputFormat;

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
