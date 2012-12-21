/*
     Copyright © 2011-2012 MLstate
 
     This file is part of Opa.
 
     Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 
     The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 
     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/
// WARNING: This file has been generated by apigen.opa, DO NOT EDIT.
// Config file: as.apigen
// Module name: AS
// Endianness: be
// OAuth: false
// Style: JS-like
import stdlib.core
import stdlib.core.wbxml
import stdlib.io.socket
import stdlib.apis.common
import stdlib.apis.apigenlib
import stdlib.apis.activesync
ALX = ApigenLibXml
type AS.SPDT = {
  int ServerId,
  int ParentId,
  string DisplayName,
  int Type
}

type AS.Changes = {list(AS.AddUpdateDelete) Changes}

type AS.ServerId = {int ServerId}

type AS.AddUpdateDelete = 
     {AS.SPDT Add}
  or {AS.SPDT Update}
  or {AS.ServerId Delete}

type AS.sync_options = {
  string User,
  string DeviceType,
  string DeviceId,
  string Cmd
}

type AS.sendmail_options = {
  string User,
  string DeviceType,
  string DeviceId,
  string Cmd
}

type AS.provision_with_key_options = {
  string User,
  string DeviceType,
  string DeviceId,
  string Cmd
}

type AS.provision_options = {
  string User,
  string DeviceType,
  string DeviceId,
  string Cmd
}

type AS.foldersync_options = {
  string User,
  string DeviceType,
  string DeviceId,
  string Cmd
}

type AS.autodiscover_options = {}

module AS {

  AS.sync_options sync_default = {User:"", DeviceType:"", DeviceId:"", Cmd:""}
  AS.sendmail_options sendmail_default = {User:"", DeviceType:"", DeviceId:"", Cmd:""}
  AS.provision_with_key_options provision_with_key_default = {User:"", DeviceType:"", DeviceId:"", Cmd:""}
  AS.provision_options provision_default = {User:"", DeviceType:"", DeviceId:"", Cmd:""}
  AS.foldersync_options foldersync_default = {User:"", DeviceType:"", DeviceId:"", Cmd:""}
  function sync_options(AS.sync_options options) {
    ApigenLib.params([{sreq:("User",options.User)},{sreq:("DeviceType",options.DeviceType)},{sreq:("DeviceId",options.DeviceId)},{sreq:("Cmd",options.Cmd)}])
  }

  function sendmail_options(AS.sendmail_options options) {
    ApigenLib.params([{sreq:("User",options.User)},{sreq:("DeviceType",options.DeviceType)},{sreq:("DeviceId",options.DeviceId)},{sreq:("Cmd",options.Cmd)}])
  }

  function provision_with_key_options(AS.provision_with_key_options options) {
    ApigenLib.params([{sreq:("User",options.User)},{sreq:("DeviceType",options.DeviceType)},{sreq:("DeviceId",options.DeviceId)},{sreq:("Cmd",options.Cmd)}])
  }

  function provision_options(AS.provision_options options) {
    ApigenLib.params([{sreq:("User",options.User)},{sreq:("DeviceType",options.DeviceType)},{sreq:("DeviceId",options.DeviceId)},{sreq:("Cmd",options.Cmd)}])
  }

  function foldersync_options(AS.foldersync_options options) {
    ApigenLib.params([{sreq:("User",options.User)},{sreq:("DeviceType",options.DeviceType)},{sreq:("DeviceId",options.DeviceId)},{sreq:("Cmd",options.Cmd)}])
  }

  gettag_SPDT = ALX.get_rec(_,{ServerId:0, ParentId:0, DisplayName:"", Type:0},function (tag,r,content) {match (tag) {
 case "ServerId": ALX.dorec(r,ALX.gettag_int,function (AS.SPDT r,int ServerId) {~{r with ServerId}},content)
 case "ParentId": ALX.dorec(r,ALX.gettag_int,function (AS.SPDT r,int ParentId) {~{r with ParentId}},content)
 case "DisplayName": ALX.dorec(r,ALX.gettag_string,function (AS.SPDT r,string DisplayName) {~{r with DisplayName}},content)
 case "Type": ALX.dorec(r,ALX.gettag_int,function (AS.SPDT r,int Type) {~{r with Type}},content)
 case _: none
 }})
  gettag_Changes = ALX.get_rec(_,{Changes:[]},function (tag,r,content) {match (tag) {
 case "Changes":
     ALX.dorec(r,ApigenLibXml.get_list(_,gettag_AddUpdateDelete),function (AS.Changes r,list(AS.AddUpdateDelete) Changes) {~{r with Changes}},content)
 case _: none
 }})
  gettag_ServerId = ALX.get_alt(_,function (tag,content) {match (tag) {
 case "ServerId": ALX.doalt(ALX.gettag_int,function (int ServerId) {{~ServerId}},content)
 case _: none
 }})
  gettag_AddUpdateDelete = ALX.get_alt(_,function (tag,content) {match (tag) {
 case "Add": ALX.doalt(gettag_SPDT,function (AS.SPDT Add) {{~Add}},content)
 case "Update": ALX.doalt(gettag_SPDT,function (AS.SPDT Update) {{~Update}},content)
 case "Delete": ALX.doalt(gettag_ServerId,function (AS.ServerId Delete) {{~Delete}},content)
 case _: none
 }})
  
  function outcome(string,string) pack_Sync(int collection_id,int synckey) {
    msg = xmlns @unsafe_cast(
<Sync xmlns="AirSync"><Collections><Collection><SyncKey>{synckey}</SyncKey><CollectionId>{collection_id}</CollectionId><GetChanges/><DeletesAsMoves/></Collection></Collections></Sync>
)
    Outcome.map_success(function (ctxt) {%%bslBinary.to_encoding%%(ctxt.buf,"binary")})(WBXml.of_xmlns({ActiveSyncDefs.context with debug:2},msg,183))
  }

  
  function outcome(string,string) pack_SendMail(string mime,string _client_id) {
    msg = xmlns @unsafe_cast(
//XMLPARAM account_id option(string)
//XMLPARAM save_in_sent_items bool
<SendMail xmlns="ComposeMail:"><ClientId>123456789</ClientId><Mime>{mime}</Mime></SendMail>
)
    Outcome.map_success(function (ctxt) {%%bslBinary.to_encoding%%(ctxt.buf,"binary")})(WBXml.of_xmlns({ActiveSyncDefs.context with debug:2},msg,163))
  }

  
  function outcome(string,string) pack_ProvisionWithKey(int status,int policy_key) {
    msg = xmlns @unsafe_cast(
<Provision xmlns="Provision:"><Policies><Policy><PolicyType>MS-EAS-Provisioning-WBXML</PolicyType><PolicyKey>{policy_key}</PolicyKey><Status>{status}</Status></Policy></Policies></Provision>
)
    Outcome.map_success(function (ctxt) {%%bslBinary.to_encoding%%(ctxt.buf,"binary")})(WBXml.of_xmlns({ActiveSyncDefs.context with debug:2},msg,190))
  }

  
  function outcome(string,string) pack_Provision(ApigenLib.simple_seq params) {
    msg = xmlns @unsafe_cast(
<Provision xmlns="Provision:" xmlns:settings="Settings:"><settings:DeviceInformation><settings:Set>{ApigenLibXml.make_simple_sequence("settings",params)}</settings:Set></settings:DeviceInformation><Policies><Policy><PolicyType>MS-EAS-Provisioning-WBXML</PolicyType></Policy></Policies></Provision>
)
    Outcome.map_success(function (ctxt) {%%bslBinary.to_encoding%%(ctxt.buf,"binary")})(WBXml.of_xmlns({ActiveSyncDefs.context with debug:2},msg,297))
  }

  
  function outcome(string,string) pack_FolderSync(int key) {
    msg = xmlns @unsafe_cast(
<FolderSync xmlns="FolderHierarchy"><SyncKey>{key}</SyncKey></FolderSync>
)
    Outcome.map_success(function (ctxt) {%%bslBinary.to_encoding%%(ctxt.buf,"binary")})(WBXml.of_xmlns({ActiveSyncDefs.context with debug:2},msg,73))
  }

  
  function outcome(string,string) pack_Autodiscover(string email) {
    msg = xmlns @unsafe_cast(
<Autodiscover xmlns="http://schemas.microsoft.com/exchange/autodiscover/outlook/requestschema/2006">
  <Request>
    <EMailAddress>{email}</EMailAddress>
    <AcceptableResponseSchema>http://schemas.microsoft.com/exchange/autodiscover/outlook/responseschema/2006a</AcceptableResponseSchema>
  </Request>
</Autodiscover>
)
    {success:"<?xml version=\"1.0\"?>\n"^Xmlns.to_string(msg)}
  }

  function sync(AS.sync_options options,string endpoint,ApigenLib.auth auth,list(string) headers,int collection_id,int synckey) {
    path = "/Microsoft-Server-ActiveSync"
    options = sync_options(options)
    content = pack_Sync(collection_id,synckey)
    match (content) {
    case {success:content}:
        ApigenLib.POST_WBXML(endpoint,path,options,auth,headers,content,ApigenLib.build_from_content_type(_,some(ActiveSyncDefs.context)))
    case {~failure}: {failure:{pack:failure}}
    }
  }

  function sendmail(AS.sendmail_options options,string endpoint,ApigenLib.auth auth,list(string) headers,string mime,string _client_id) {
    path = "/Microsoft-Server-ActiveSync"
    options = sendmail_options(options)
    content = pack_SendMail(mime,_client_id)
    match (content) {
    case {success:content}:
        ApigenLib.POST_WBXML(endpoint,path,options,auth,headers,content,ApigenLib.build_from_content_type(_,some(ActiveSyncDefs.context)))
    case {~failure}: {failure:{pack:failure}}
    }
  }

  function provision_with_key(AS.provision_with_key_options options,string endpoint,ApigenLib.auth auth,list(string) headers,int status,int policy_key) {
    path = "/Microsoft-Server-ActiveSync"
    options = provision_with_key_options(options)
    content = pack_ProvisionWithKey(status,policy_key)
    match (content) {
    case {success:content}:
        ApigenLib.POST_WBXML(endpoint,path,options,auth,headers,content,ApigenLib.build_from_content_type(_,some(ActiveSyncDefs.context)))
    case {~failure}: {failure:{pack:failure}}
    }
  }

  function provision(AS.provision_options options,string endpoint,ApigenLib.auth auth,list(string) headers,ApigenLib.simple_seq params) {
    path = "/Microsoft-Server-ActiveSync"
    options = provision_options(options)
    content = pack_Provision(params)
    match (content) {
    case {success:content}:
        ApigenLib.POST_WBXML(endpoint,path,options,auth,headers,content,ApigenLib.build_from_content_type(_,some(ActiveSyncDefs.context)))
    case {~failure}: {failure:{pack:failure}}
    }
  }

  function foldersync(AS.foldersync_options options,string endpoint,ApigenLib.auth auth,list(string) headers,int key) {
    path = "/Microsoft-Server-ActiveSync"
    options = foldersync_options(options)
    content = pack_FolderSync(key)
    match (content) {
    case {success:content}:
        ApigenLib.POST_WBXML(endpoint,path,options,auth,headers,content,ApigenLib.build_from_content_type(_,some(ActiveSyncDefs.context)))
    case {~failure}: {failure:{pack:failure}}
    }
  }

  function autodiscover(string endpoint,ApigenLib.auth auth,list(string) headers,string email) {
    path = "/autodiscover/autodiscover.xml"
    options = []
    content = pack_Autodiscover(email)
    match (content) {
    case {success:content}:
        ApigenLib.POST_XML(endpoint,path,options,auth,headers,content,ApigenLib.build_from_content_type(_,some(ActiveSyncDefs.context)))
    case {~failure}: {failure:{pack:failure}}
    }
  }


}

// End of AS