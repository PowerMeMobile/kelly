var baseServiceUrl = "http://10.10.0.155:8080";

function IsInteger(str){
    return parseInt(str) == str;
};

function IsIPAddressValid(ip){
    var regex = "\\b(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\b";
    return ip.match(regex);
};

function IsSourceValid(source){
    var regex = '^(\\d+\\,\\d+\\,\\d+){1}(;{1}\\d+\\,\\d+\\,\\d+)*;?$';
    return source.match(regex);
};

