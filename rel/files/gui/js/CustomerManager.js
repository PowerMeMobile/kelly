var CustomerManager = jQuery.Class.create({
    init: function (baseUrl) {
        this.baseUrl = baseUrl;
    },

    getCustomer: function (id, outputFormat, responseContainer) {
        responseContainer.val("");
        if(id == ""){
            alert("Please specify customer id");
            return;
        }
        var url = this.baseUrl + "/customer/" + encodeURIComponent(id) + "?view=" + outputFormat;

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

    createCustomer: function (id,uuid,priority,rps,allowedSources,defaultSource,networks,defaultProviderId,allowReceipts,noRetry,defaultValidity,maxValidity,outputFormat,responseContainer) {
        responseContainer.val("");

        if(id == ''){
            alert("Please specify customer id");
            return;
        }
        if(uuid == ''){
            alert("Please specify customer UUID");
            return;
        }
        if(priority == ''){
            alert("Please specify the priority");
            return;
        }
        if(allowedSources == ''){
            alert("Please specify allowed sources");
            return;
        }
        if(networks == ''){
            alert("Please specify network IDs separated by a comma");
            return;
        }
        if(maxValidity == ''){
            alert("Please specify max validity");
            return;
        }
        if(!IsInteger(priority)){
            alert("Priority should be integer");
            return;
        }
        if(rps != '' && !IsInteger(rps)){
            alert("RPS should be integer");
            return;
        }
        if(defaultValidity!='' && !IsInteger(defaultValidity)){
            alert("Default validity should be integer");
            return;
        }
        if(!IsInteger(maxValidity)){
            alert("Max validity should be integer");
            return;
        }
        if(!IsSourceValid(allowedSources)){
            alert("'Allowed sources' parameter is not valid. It must have the following format: separated by a semicolon sources (ex.'source1;source2') where source - comma-delimited list of integers: addr, ton, npi (ex.'375292707271,0,0')");
            return;
        }
        if(defaultSource!='' && !IsSourceValid(defaultSource)){
            alert("'Default source' parameter is not valid. It must be filled with comma-separated list of integers: addr, ton, npi (ex.'375292707271,0,0')");
            return;
        }

        var url = this.baseUrl+"/customer?view="+outputFormat;

        $.ajax({
            thisObj:this,
            type:"POST",
            data: ["id=",encodeURIComponent(id),"&uuid=",encodeURIComponent(uuid),"&priority=",priority, rps!=""?"&rps="+rps:"",
                "&allowed_sources=",allowedSources, defaultSource!=""?"&default_source="+defaultSource:"","&networks=",encodeURIComponent(networks),"&receipts_allowed=",allowReceipts,
                "&no_retry=",noRetry,"&default_validity=",defaultValidity,"&max_validity=",maxValidity].join(""),
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

    deleteCustomer: function (id, outputFormat, responseContainer){
        responseContainer.val('');
        if(id == ''){
            alert("Please specify customer id");
            return;
        }

        var url = this.baseUrl+"/customer/"+encodeURIComponent(id)+"?view="+outputFormat;

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
