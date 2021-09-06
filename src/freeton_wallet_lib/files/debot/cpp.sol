
#define _F(f) tvm.functionId(f)
#define F_(f) tvm.functionId(f)

// Use this macro for time+expire+pubkey headers
#define MSGINT(okCallback, errorCallback) \
        { \
          abiVer: 2, \
          extMsg: true, \
          sign: false, \
          pubkey: nopubkey, \
          time: uint64(now), \
          expire: 0, \
          callbackId: _F(okCallback), \
          onErrorId: _F(errorCallback), \
          } 

// Use this macro for time+expire headers
#define MSGINT2(okCallback, errorCallback) \
        { \
          abiVer: 2, \
          extMsg: true, \
          sign: false, \
          time: uint64(now), \
          expire: 0, \
          callbackId: _F(okCallback), \
          onErrorId: _F(errorCallback), \
          } 

#define MSGEXT(pkey,okCallback, errorCallback)  \
        { \
          abiVer: 2, \
          extMsg: true, \
          sign: true, \
          pubkey: pkey, \
          time: uint64(now), \
          expire: 0, \
          callbackId: _F(okCallback), \
          onErrorId: _F(errorCallback), \
          } 

#define FWD_GAS { value: 0, flag: 64, bounce: true }
#define SEND_ALL_GAS(dest) dest.transfer(0 , false, 64 );
#define MSG_GAS(v) { value: v, flag: 0, bounce: true}

#define PRINTF(fmt...) Terminal.print(0, format(fmt))
#define PRINTFCC(f,fmt...) Terminal.print(_F(f), format(fmt))
#define PRINT(s) Terminal.print(0, s)
#define PRINTCC(f,s) Terminal.print(_F(f), s)
#define CONFIRM(f, s) ConfirmInput.get(_F(f),s)
#define INPUT(f, s, multiline) Terminal.input(_F(f),s, multiline)
#define ASK_ADDRESS(f, s) AddressInput.get(_F(f),s)

#define CODEHASH(cc) 0x%{get-code-hash:contract:tvc:cc}

#define DEBOT_NAME(name,publisher,caption,author) \
  string constant debot_name = name ; \
  string constant debot_publisher = publisher ; \
  string constant debot_caption = caption ; \
  string constant debot_author = author ; \
  string constant debot_language = "en" 

#define DEBOT_VERSION(major, minor, fix) \
  uint8 constant debot_version_major = major ; \
  uint8 constant debot_version_minor = minor ; \
  uint8 constant debot_version_fix = fix 

#define DEBOT_SUPPORT(addr) \
  uint256 constant debot_support = addr 

#define DEBOT_HELLO(s) \
  string constant debot_hello = s

#define MAP_ITER(map, key, key_type, value, value_type, code)    \
  optional (key_type, value_type) map##_iter = map.min() ;       \
  while( map##_iter.hasValue() ){                                \
    ( key_type key, value_type value ) = map##_iter.get() ;      \
    code                                                         \
    map##_iter = map.next( key );                                \
  } map##_iter

#define QUERY_HEXA_FUNCTION(f,text,pubkey,code)                 \
  function f() internal                                         \
  {                                                             \
    Terminal.input( tvm.functionId(f##Callback), text, false ); \
  }                                                             \
                                                                \
  function f##Callback( string value ) public                           \
  {                                                                     \
    bytes s = bytes(value) ;                                            \
    if( s.length < 2 || uint8(s[0]) != 40 || uint8(s[1]) != 120 )       \
      value = "0x" + value;                                             \
    (uint256 pubkey, bool res) = stoi(value);                           \
    if( !res ){                                                         \
      PRINT("Invalid format for entry");                                \
      f();                                                              \
      return;                                                           \
    }                                                                   \
    code                                                                \
  }

#define QUERY_UINT256_FUNCTION(f,text,pubkey,code)              \
  function f() internal                                         \
  {                                                             \
    Terminal.input( tvm.functionId(f##Callback), text, false ); \
  }                                                             \
                                                                \
  function f##Callback( string value ) public                   \
  {                                                             \
    (uint256 pubkey, bool res) = stoi(value);                   \
    if( !res ){                                                 \
      PRINT("Invalid format for number");                       \
      f();                                                      \
      return;                                                   \
    }                                                           \
    code                                                        \
  }

#define QUERY_STRING_FUNCTION(f,text,variable,code)                    \
  function f() internal                                             \
  {                                                                 \
    Terminal.input( tvm.functionId(on##f##Callback), text, false ); \
  }                                                                 \
                                                                    \
  function on##f##Callback( string value ) public                   \
  {                                                                 \
    string variable = value;                                        \
    code                                                            \
  }

#define QUERY_CONTRACT_FUNCTION(f, text, g_addr, code )             \
  address g_addr;                                                   \
  function f() internal                                             \
  {                                                                 \
    AddressInput.get(F_(on##f##Callback), text);                    \
  }                                                                 \
  function on##f##Callback(address value) public                    \
  {                                                                 \
    g_addr = value ;                                                \
    Sdk.getAccountType(F_( on##f##StatusCallback ), value);         \
  }                                                                 \
  function on##f##StatusCallback(int8 acc_type) public {            \
    if (!Utility._checkActiveStatus(acc_type, "Contract")) {        \
      PRINT("Error: contract is not deployed");                     \
      f();                                                          \
    } else {                                                        \
      code                                                          \
        }                                                           \
  }

/*
  Menus:
   MENU(
     MENU_ITEM("description1", callback1);
     MENU_ITEM("description2", callback2);
     );
 */

#define MENU_WITH_TITLE(title, code)             \
  MenuItem[] items ;                             \
  code                                           \
  Menu.select(title, "", items)

#define MENU(code) MENU_WITH_TITLE("What is your choice?", code)

#define MENU_ITEM(text, f) \
  items.push( MenuItem(text, "", tvm.functionId(f)) )

#define REQUIRE_TVM_PUBKEY() \
  require( tvm.pubkey() == msg.pubkey(), 100 )

#define ACCEPT_TVM_PUBKEY() \
  REQUIRE_TVM_PUBKEY();     \
  tvm.accept()
