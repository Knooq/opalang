/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

type WBXml.context = {
  binary buf, /** The wbxml being built */
  int tag_cp, /** Current tag code page */
  int att_cp, /** Current attribute code page */
  int nsti, /** Next string table index */
  int sthresh, /** String table threshold */
  stringmap(int) st, /** String table */
  intmap(string) namespace_map, /** Map from code page to namespace */
  stringmap(int) namespace_rmap, /** Map from namespace to code page */
  intmap(string) tag_code_space, /** Map from code page/tag code to tag names */
  ordered_map((int,string),int,Order.default) reverse_tag_code_space, /** Map from code page/tag name to tag codes */
  intmap(string) att_code_space, /** Map from code page/attribute code to attribute names */
  ordered_map((int,string),int,Order.default) reverse_att_code_space /** Map from code page/attribute name to attribute codes */
}

module WBXml {

  private (binary -> string) bindump = %% BslPervasives.bindump %%

  MIBE_US_ASCI    = 3
  MIBE_ISO_8859_1 = 4
  MIBE_UTF8       = 106

  SWITCH_PAGE = 0x00
  END         = 0x01
  ENTITY      = 0x02
  STR_I       = 0x03
  LITERAL     = 0x04
  EXT_I_0     = 0x40
  EXT_I_1     = 0x41
  EXT_I_2     = 0x42
  PI          = 0x43
  LITERAL_C   = 0x44
  EXT_T_0     = 0x80
  EXT_T_1     = 0x81
  EXT_T_2     = 0x82
  STR_T       = 0x83
  LITERAL_A   = 0x84
  EXT_0       = 0xC0
  EXT_1       = 0xC1
  EXT_2       = 0xC2
  OPAQUE      = 0xC3
  LITERAL_AC  = 0xC4

  EN_TYPE             = 1
  EN_TAG              = 2
  EN_CONTENT          = 3
  EN_FLAGS            = 4
  EN_ATTRIBUTES       = 5

  EN_TYPE_STARTTAG    = 1
  EN_TYPE_ENDTAG      = 2
  EN_TYPE_CONTENT     = 3

  EN_FLAGS_CONTENT    = 1
  EN_FLAGS_ATTRIBUTES = 2

  private function Order.ordering order_is((int,string) (i1,s1), (int,string) (i2,s2)) {
    match (Int.ordering(i1,i2)) {
    case {eq}: String.ordering(s1,s2);
    case ord: ord;
    }
  }
  private isMap = Map_make(order((int,string),Order.default) Order.make(order_is))

  private function map_from_code_space(code_space) {
    List.fold(function ((cp, codes), dcm) {
                List.fold(function ((code, tag), dcm) {
                            IntMap.add(cp * 0x100 + code, tag, dcm)
                          },codes,dcm)
              },code_space,IntMap.empty)
  }

  function get_tag_from_code(ctxt, cp, code) { IntMap.get(cp * 0x100 + code, ctxt.tag_code_space) }
  function get_att_from_code(ctxt, cp, code) { IntMap.get(cp * 0x100 + code, ctxt.att_code_space) }

  private function reverse_map_from_code_space(code_space) {
    List.fold(function ((cp, codes), dcm) {
                List.fold(function ((code, tag), dcm) {
                            isMap.add((cp,tag), code, dcm)
                          },codes,dcm)
              },code_space,isMap.empty)
  }

  function get_code_from_tag(ctxt, cp, tag) { isMap.get((cp,tag), ctxt.reverse_tag_code_space) }
  function get_code_from_att(ctxt, cp, att) { isMap.get((cp,att), ctxt.reverse_att_code_space) }

  function cstring(ctxt, str) {
    Binary.add_string(ctxt.buf,str)
    Binary.add_uint8(ctxt.buf,0x00)
    ctxt
  }

  function start(ctxt) {
    Binary.add_uint8(ctxt.buf,0x03) // WBXML 1.3
    Binary.add_uint8(ctxt.buf,0x01) // Public ID 1
    Binary.add_uint8(ctxt.buf,MIBE_UTF8) // UTF-8
    if (StringMap.is_empty(ctxt.st))
      Binary.add_uint8(ctxt.buf,0x00) // string table length (0)
    else {
      stlen = StringMap.fold(function (str, _, stlen) { stlen + String.length(str) + 1 },ctxt.st,0)
      Binary.add_binary(ctxt.buf,MBInt.of_int(stlen))
      StringMap.iter(function (str, _) { ignore(cstring(ctxt, str)) },ctxt.st)
    }
    ctxt
  }

  private function get_mapping(tagatt, ctxt, ta, namespace) {
    cp =
      if (namespace == "")
        0
      else
        match (StringMap.get(namespace, ctxt.namespace_rmap)) {
        case {some:cp}: cp;
        default: 0;
        }
    //jlog("ta={ta}, cp={cp}")
    match (match (tagatt) {
           case {tag}: get_code_from_tag(ctxt, cp, ta);
           case {att}: get_code_from_att(ctxt, cp, ta);
           }) {
    case {some:code}: {some:(cp, code)};
    default: none;
    }
  }

  function end_tag(ctxt) {
    Binary.add_uint8(ctxt.buf, END)
    ctxt
  }

  function switch_page(ctxt, tagatt, cp) {
    (at_cp,set) =
      match (tagatt) {
      case {tag}: (ctxt.tag_cp,function (ctxt,cp) { {ctxt with tag_cp:cp} });
      case {att}: (ctxt.att_cp,function (ctxt,cp) { {ctxt with att_cp:cp} });
      }
    if (at_cp != cp) {
      Binary.add_uint8(ctxt.buf,SWITCH_PAGE);
      Binary.add_uint8(ctxt.buf,cp);
      set(ctxt,cp)
    } else ctxt
  }

  function attributes(ctxt, namespace, atts) {
    jlog("attributes: {atts}")
    ctxt =
      if (atts != [])
        List.fold(function (att, ctxt) {
                    match
                      (match (get_mapping({att}, ctxt, att.name, namespace)) {
                       case {some:(cp,code)}:
                         jlog("{att.name}: cp={cp} code={code}")
                         if (code < 128) {
                           ctxt = switch_page(ctxt, {att}, cp)
                           Binary.add_uint8(ctxt.buf, code); (ctxt,true)
                         } else (ctxt,false);
                       case {none}:
                         match (StringMap.get(att.name,ctxt.st)) {
                         case {some:idx}: ignore(literal(ctxt,idx)); (ctxt,true);
                         case {none}: (ctxt,false);
                         }
                       }) {
                    case (ctxt,{true}):
                      match (get_mapping({att}, ctxt, att.value, namespace)) {
                      case {some:(cp,code)}:
                        jlog("{att.value}: cp={cp} code={code}")
                        if (code >= 128) {
                          ctxt = switch_page(ctxt, {att}, cp)
                          Binary.add_uint8(ctxt.buf, code)
                          ctxt
                        } else
                          cstring(ctxt, att.value);
                      case {none}:
                        cstring(ctxt, att.value);
                      }
                    case (ctxt,{false}): ctxt;
                    }
                  },atts,ctxt)
      else ctxt
    end_tag(ctxt)
  }

  function content(ctxt, string content) {
    Binary.add_uint8(ctxt.buf,STR_I)
    cstring(ctxt, content)
  }

  function stref(ctxt, int idx) {
    Binary.add_uint8(ctxt.buf,STR_T)
    Binary.add_binary(ctxt.buf,MBInt.of_int(idx))
    ctxt
  }

  function literal(ctxt, int idx) {
    Binary.add_uint8(ctxt.buf,LITERAL)
    Binary.add_binary(ctxt.buf,MBInt.of_int(idx))
    ctxt
  }

  function start_tag(ctxt, tag, namespace, atts, content) {
    match (get_mapping({tag}, ctxt, tag, namespace)) {
    case {some:(cp,code)}:
      jlog("cp={cp} code={code}")
      ctxt = switch_page(ctxt, {tag}, cp)
      code = if (List.length(atts) > 0) Bitwise.lor(code,0x80) else code
      code = if (List.length(content) > 0) Bitwise.lor(code,0x40) else code
      Binary.add_uint8(ctxt.buf,code)
      attributes(ctxt, namespace, atts);
    case {none}:
      match (StringMap.get(tag,ctxt.st)) {
      case {some:idx}:
        idx = if (List.length(atts) > 0) Bitwise.lor(idx,0x80) else idx
        idx = if (List.length(content) > 0) Bitwise.lor(idx,0x40) else idx
        ctxt = literal(ctxt, idx)
        attributes(ctxt, namespace, atts);
      case {none}: ctxt;
      }
    }
  }

  function of_xmlns0(ctxt, xmlns xmlns) {
    match (xmlns) {
      case {text:""}: jlog("text:\"\""); {success:ctxt};
      case {~text}: // *MUST NOT* have null characters
        jlog("text:\"{String.replace("\n","\\n",text)}\"");
        match (StringMap.get(text, ctxt.st)) {
        case {some:idx}: {success:stref(ctxt, idx)};
        case {none}: {success:content(ctxt, text)};
        }
      case {args:[], content:[], ~namespace, specific_attributes:_, xmlns:_, ~tag}:
        jlog("lone tag: {tag}")
        {success:start_tag(ctxt, tag, namespace, [], [])}
      case {~args, ~content, ~namespace, specific_attributes:_, xmlns:_, ~tag}:
        jlog("tag: {tag}")
        ctxt = start_tag(ctxt, tag, namespace, args, content)
        match (List.fold(function (xmlns, ctxt) {
                           match (ctxt) {
                           case {success:ctxt}: of_xmlns0(ctxt, xmlns);
                           case {~failure}: {~failure};
                         }},content,{success:ctxt})) {
        case {success:ctxt}:
          ctxt = end_tag(ctxt)
          {success:ctxt};
        case {~failure}: {~failure};
        }
      case {content_unsafe:_}: {failure:"content_unsafe"};
      case {fragment:_}: {failure:"fragment"};
      case {xml_dialect:_}: {failure:"xml_dialect"};
    }
  }

  function add_to_string_table(ctxt, string text) {
    if (String.length(text) >= ctxt.sthresh)
      {ctxt with st:StringMap.add(text,ctxt.nsti,ctxt.st), nsti:ctxt.nsti+1}
    else
      ctxt
  }

  function string_table_of_attributes(ctxt, namespace, atts) {
    List.fold(function (att, ctxt) {
                match (get_mapping({att}, ctxt, att.name, namespace)) {
                case {some:_}: ctxt;
                case {none}: add_to_string_table(ctxt, att.name);
                }
              },atts,ctxt)
  }

  function string_table_of_xmlns(ctxt, xmlns xmlns) {
    match (xmlns) {
      case {text:""}: ctxt;
      case {~text}: add_to_string_table(ctxt, text);
      case {content_unsafe:_}: ctxt;
      case {fragment:_}: ctxt;
      case {xml_dialect:_}: ctxt;
      case {~args, ~content, ~namespace, specific_attributes:_, xmlns:_, ~tag}:
        ctxt =
          match (get_mapping({tag}, ctxt, tag, namespace)) {
          case {none}: add_to_string_table(ctxt, tag);
          case {some:_}: ctxt;
          }
        ctxt = string_table_of_attributes(ctxt, namespace, args)
        List.fold(function (xmlns, ctxt) { string_table_of_xmlns(ctxt, xmlns) },content,ctxt)
    }
  }

  function WBXml.context init_context(int sthresh, namespaces, tag_code_space, att_code_space) {
    {
      buf:Binary.create(0), tag_cp:0, att_cp:0, nsti:0, ~sthresh, st:StringMap.empty,
      namespace_map:List.fold(function ((code, tag), nm) { IntMap.add(code, tag, nm) },namespaces,IntMap.empty),
      namespace_rmap:List.fold(function ((code, tag), nm) { StringMap.add(tag, code, nm) },namespaces,StringMap.empty),
      tag_code_space:map_from_code_space(tag_code_space),
      reverse_tag_code_space:reverse_map_from_code_space(tag_code_space),
      att_code_space:map_from_code_space(att_code_space),
      reverse_att_code_space:reverse_map_from_code_space(att_code_space)
    }
  }

  function of_xmlns(WBXml.context ctxt, xmlns xmlns, int hint) {
    jlog("of_xmlns: xmlns={xmlns}");
    ctxt = { ctxt with buf:Binary.create(hint), tag_cp:0, att_cp:0, nsti:0, st:StringMap.empty }
    ctxt = string_table_of_xmlns(ctxt, xmlns)
    ctxt = start(ctxt)
    of_xmlns0(ctxt, xmlns)
  }

}

