/*
     Copyright © 2011-2012 MLstate
 
     This file is part of Opa.
 
     Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 
     The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 
     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/
import stdlib.io.socket
import stdlib.apis.common
import stdlib.apis.apigenlib

type ActiveSync.conn = {
  string asversion,
  string endpoint,
  string user,
  string password,
  string email,
  string device_type,
  string device_id,
  ApigenLib.simple_seq device_information,
  string display_name,
  int policy_key,
  list(string) owaurls,
  option(AS.Changes) changes,
  stringmap(xmlns) stored_xml,
  stringmap(int) stored_statuses,
  option(xml_document) autodiscover_xmldoc,
  Apigen.content content,
  option(Apigen.failure) error
}

module ActiveSync {

  private get_elements = ApigenLibXml.get_xml_elements
  private get_string = ApigenLibXml.get_xml_string
  private get_int = ApigenLibXml.get_xml_int

  private all_chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-"
  function make_device_id() { Random.generic_string("0123456789ABCDEF",32) }
  function make_imei() { Random.generic_string(all_chars,15) }
  function make_client_id() { Random.generic_string(all_chars,40) }

  function ActiveSync.conn init(string asversion, string endpoint, string user, string password, string email,
                                string device_type, string device_id, ApigenLib.simple_seq device_information) {
    ~{ asversion, endpoint, user, password, email, device_type, device_id, device_information,
       policy_key:0, display_name:"", owaurls:[], changes:none,
       stored_xml:StringMap.empty, stored_statuses:StringMap.empty,
       autodiscover_xmldoc:none,
       content:{none}, error:none
     }
  }

  function reprovision(conn) {
    conn = provision(conn)
    if (Option.is_some(conn.error))
      conn
    else {
      jlog("provision xml: {Option.map(Xmlns.to_string,get_xml(conn,"provision"))}")
      match (get_status(conn,"provision")) {
      case {some:1}:
        jlog("temporary policy key: {Ansi.print({yellow},"{conn.policy_key}")}")
        conn = provision_with_key(conn, 1, conn.policy_key)
        if (Option.is_some(conn.error))
          conn
        else {
          jlog("final policy key: {Ansi.print({yellow},"{conn.policy_key}")}")
          conn = foldersync(conn)
          if (Option.is_some(conn.error))
            conn
          else
            get_changes(conn)
        }
      case {some:status}: {conn with error:{some:{error:"Unknown Provision Status {status}"}}};
      case {none}: {conn with error:{some:{error:"No Provision Status"}}};
      }
    }
  }

  function ActiveSync.conn initialise(ActiveSync.conn conn) {
    conn = autodiscover(conn)
    if (Option.is_some(conn.error))
      conn
    else {
      conn = foldersync(conn)
      if (Option.is_some(conn.error))
        conn
      else {
        match (get_status(conn,"foldersync")) {
        case {some:1}: get_changes(conn);
        case {some:143}  // Policy refresh
        case {some:144}: // Needs provisioning
          reprovision(conn);
        case {some:status}: {conn with error:{some:{error:"Unknown FolderSync Status {status}"}}};
        case {none}: {conn with error:{some:{error:"No FolderSync Status"}}};
        }
      }
    }
  }

  private function install(conn,name,get,path,set,xmlns,min,max) {
    path = Option.get(ApigenLibXml.parse_path(path))
    match (get(path,xmlns)) {
    case {success:[]}:
      if (min <= 0) {
        set({conn with error:none},[])
      } else {
        {conn with error:{some:{error:"ActiveSync.{name}: no value"}}}
      }
    case {success:[value]}:
      if (min <= 1 && max >= 1) {
        set({conn with error:none},[value])
      } else {
        {conn with error:{some:{error:"ActiveSync.{name}: single value {value}"}}}
      }
    case {success:values}:
      no_values = List.length(values)
      if (min > no_values) {
        {conn with error:{some:{error:"ActiveSync.{name}: not enough values {values}"}}}
      } else if (max < no_values) {
        {conn with error:{some:{error:"ActiveSync.{name}: too many values {values}"}}}
      } else {
        set({conn with error:none},values)
      }
    case {~failure}:
      {conn with error:{some:{error:failure}}}
    }
  }

  private function ActiveSync.conn store_xmlns(ActiveSync.conn conn, Apigen.content content, string name) {
    function ActiveSync.conn remove(conn) { {conn with stored_xml:StringMap.remove(name,conn.stored_xml)} }
    match (content) {
    case {~xmldoc}: {conn with ~content, stored_xml:StringMap.add(name,xmldoc.element,conn.stored_xml), error:none};
    case {~xmlns}: {conn with ~content, stored_xml:StringMap.add(name,xmlns,conn.stored_xml), error:none};
    case {~xhtml}: {remove(conn) with error:{some:{error_html:xhtml}}};
    default: {remove(conn) with error:{some:{bad_content:content}}};
    }
  }

  private function ActiveSync.conn store_status(ActiveSync.conn conn, list(int) statuses, string name, int dflt) {
    //jlog("store_status: statuses:{statuses}  name:{name}")
    {conn with stored_statuses:StringMap.add(name,Option.default(dflt,List.nth(0,statuses)),conn.stored_statuses)}
  }

  function option(xmlns) get_xml(ActiveSync.conn conn, string name) { StringMap.get(name,conn.stored_xml) }
  function option(int) get_status(ActiveSync.conn conn, string name) { StringMap.get(name,conn.stored_statuses) }

  function ActiveSync.conn get_display_name(ActiveSync.conn conn) {
    match (conn.autodiscover_xmldoc) {
    case {some:xmldoc}:
      install(conn,"get_display_name",get_string,"Autodiscover.Response.User.DisplayName",
              function (conn, names) { {conn with display_name:Option.default("",List.nth(0,names))} },xmldoc.element,1,1)
    case {none}:
      conn = autodiscover(conn)
      if (Option.is_some(conn.error) || Option.is_none(conn.autodiscover_xmldoc)) conn else get_display_name(conn);
    }
  }

  function ActiveSync.conn get_owaurl(ActiveSync.conn conn) {
    match (conn.autodiscover_xmldoc) {
    case {some:xmldoc}:
      install(conn,"get_owaurl",get_string,
              "Autodiscover.Response.Account.Protocol[Type=\"WEB\"].Internal.OWAUrl",
              function (conn, owaurls) { {conn with ~owaurls } },xmldoc.element,1,100)
    case {none}:
      conn = autodiscover(conn)
      if (Option.is_some(conn.error) || Option.is_none(conn.autodiscover_xmldoc)) conn else get_owaurl(conn);
    }
  }

  function ActiveSync.conn get_changes(ActiveSync.conn conn) {
    match (StringMap.get("foldersync",conn.stored_xml)) {
    case {some:xmlns}:
      match (get_elements(Option.get(ApigenLibXml.parse_path("FolderSync")),xmlns)) {
      case {success:foldersync}:
        //jlog("get_changes: foldersync={String.concat("\n",List.map(Xmlns.to_string,foldersync))}")
        option(AS.Changes) changes = AS.gettag_Changes(foldersync)
        jlog("changes:{changes}");
        {conn with ~changes, error:none};
      case {~failure}: {conn with changes:none, error:{some:{error:failure}}};
      }
    default: {conn with changes:none, error:{some:{error:"ActiveSync.get_changes: no foldersync XML"}}};
    }
  }

  /** Perform the autodiscover routine.
   *
   * We don't actually go through Microsoft's ridiculous palaver to get
   * at the autodiscover service.  We assume our endpoint is valid.
   * In future we might handle redirect, alternate addresses etc.
   */
  function ActiveSync.conn autodiscover(ActiveSync.conn conn) {
    match (AS.autodiscover(conn.endpoint,{user:conn.user,password:conn.password},[],conn.email)) {
    case {success:{~xmldoc}}:
      /*API_libs_private.api*/jlog("ActiveSync.autodiscover: xml=\n{Xmlns.to_string(xmldoc.element)}")
      {conn with autodiscover_xmldoc:{some:xmldoc}, error:none};
    case {success:{~xmlns}}: {conn with error:{some:{bad_content:{~xmlns}}}};
    case {success:{~xhtml}}: {conn with error:{some:{bad_content:{~xhtml}}}};
    case {success:{~plain}}: {conn with error:{some:{bad_content:{~plain}}}};
    case {~failure}: {conn with error:{some:failure}};
    }
  }

  private function asheaders(conn) { ["X-MS-PolicyKey: {conn.policy_key}", "MS-ASProtocolVersion: {conn.asversion}"] }

  // TODO: remote wipe
  // TODO: analyse hierarchy
  function ActiveSync.conn foldersync(ActiveSync.conn conn) {
    key = Option.default(0,StringMap.get("synckey",conn.stored_statuses))
    match (AS.foldersync({User:conn.user, DeviceType:conn.device_type, DeviceId:conn.device_id, Cmd:"FolderSync"},
                         conn.endpoint,{user:conn.user,password:conn.password},asheaders(conn),key)) {
    case {success:content}:
      conn = store_xmlns(conn, content, "foldersync")
      match (content) {
      case {~xmlns}:
        conn = install(conn,"foldersync",get_int,"FolderSync.Status",store_status(_,_,"foldersync",-1),xmlns,1,1)
        install(conn,"foldersync",get_int,"FolderSync.SyncKey",store_status(_,_,"synckey",0),xmlns,0,1)
      default: conn;
      }
    case {~failure}: {conn with error:{some:failure}};
    }
  }

  private function provision_(conn, res) {
    match (res) {
    case {success:content}:
      conn = store_xmlns(conn, content, "provision")
      match (content) {
      case {~xmlns}:
        conn = install(conn,"provision_with_key",get_int,"Provision.Policies.Policy.Status",
                       store_status(_,_,"provision",-1),xmlns,1,1)
        install(conn,"provision",get_int,"Provision.Policies.Policy.PolicyKey", // <-- BEWARE: this might be 64-bit
                function (conn, keys) { {conn with policy_key:Option.default(0,List.nth(0,keys))} },xmlns,1,1);
      default: conn;
      }
    case {~failure}: {conn with error:{some:failure}};
    }
  }

  function ActiveSync.conn provision(ActiveSync.conn conn) {
    provision_(conn,AS.provision({User:conn.user, DeviceType:conn.device_type, DeviceId:conn.device_id, Cmd:"Provision"},
                                 conn.endpoint,{user:conn.user,password:conn.password},asheaders(conn),conn.device_information))
  }

  function ActiveSync.conn provision_with_key(ActiveSync.conn conn, int status, int policy_key) {
    provision_(conn,AS.provision_with_key({User:conn.user, DeviceType:conn.device_type, DeviceId:conn.device_id, Cmd:"Provision"},
                                          conn.endpoint,{user:conn.user,password:conn.password},
                                          asheaders(conn),status,policy_key))
  }

  function ActiveSync.conn sendmail(ActiveSync.conn conn,
                                    string mime, /*bool save_in_sent_items, option(string) account_id,*/ string client_id) {
    match (AS.sendmail({User:conn.user, DeviceType:conn.device_type, DeviceId:conn.device_id, Cmd:"SendMail"},
                       conn.endpoint,{user:conn.user,password:conn.password},asheaders(conn),
                       mime,/*save_in_sent_items,account_id,*/client_id)) {
    case {success:content}:
      conn = store_xmlns(conn, content, "sendmail")
      match (content) {
      case {~xmlns}: _ = xmlns
        //conn = install(conn,"foldersync",get_int,"FolderSync.Status",store_status(_,_,"foldersync",-1),xmlns,1,1)
        conn
      default: conn;
      }
    case {~failure}: {conn with error:{some:failure}};
    }
  }

  function ActiveSync.conn sync(ActiveSync.conn conn, int collection_id) {
    match (AS.sync({User:conn.user, DeviceType:conn.device_type, DeviceId:conn.device_id, Cmd:"Sync"},
                       conn.endpoint,{user:conn.user,password:conn.password},asheaders(conn),
                       collection_id,0)) {
    case {success:content}:
      conn = store_xmlns(conn, content, "sync")
      match (content) {
      case {~xmlns}: _ = xmlns
        //conn = install(conn,"foldersync",get_int,"FolderSync.Status",store_status(_,_,"foldersync",-1),xmlns,1,1)
        conn
      default: conn;
      }
    case {~failure}: {conn with error:{some:failure}};
    }
  }

}

