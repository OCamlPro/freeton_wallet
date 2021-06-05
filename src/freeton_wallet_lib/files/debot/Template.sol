pragma ton-solidity >=0.35.0;

pragma AbiHeader expire;
pragma AbiHeader time;
pragma AbiHeader pubkey;

import "lib/Debot.sol";
import "lib/Terminal.sol";
import "lib/AddressInput.sol";
import "lib/AmountInput.sol";
import "lib/ConfirmInput.sol";
import "lib/Sdk.sol";
import "lib/Menu.sol";
import "lib/Upgradable.sol";
import "lib/Transferable.sol";

// Interface of the contract with which to interact

interface IContract {
  function setter ( uint256 x ) external ;
  function getter () external returns ( uint256 y ) ;
}

abstract contract Utility {

  function tonsToStr(uint128 nanotons) internal pure returns (string) {
    (uint64 dec, uint64 float) = _tokens(nanotons);
    string floatStr = format("{}", float);
    while (floatStr.byteLength() < 9) {
      floatStr = "0" + floatStr;
    }
    return format("{}.{}", dec, floatStr);
  }

  function _tokens(uint128 nanotokens) internal pure
    returns (uint64, uint64) {
    uint64 decimal = uint64(nanotokens / 1e9);
    uint64 float = uint64(nanotokens - (decimal * 1e9));
    return (decimal, float);
  }
}

contract !{name}Debot is Debot, Upgradable, Transferable, Utility {

  string constant debot_name = "!{name}Debot" ;
  string constant debot_publisher = "My Software Company" ;
  string constant debot_caption = "Debot of !{name} contracts" ;
  string constant debot_author = "My Software Company" ;
  string constant debot_language = "en" ;
  // your address with 0x instead of 0:
  uint8 constant debot_version_major = 1 ;
  uint8 constant debot_version_minor = 0 ;
  uint8 constant debot_version_fix = 0 ;
  uint256 constant debot_support =
    0x66e01d6df5a8d7677d9ab2daf7f258f1e2a7fe73da5320300395f99e01dc3b5f ;

  string constant debot_hello =
    "Hi, I will help you work with !{name} contracts";

  address g_contract ;
  uint128 g_balance ;
  uint32 g_retryId ;
  bytes m_icon;

  function askContractAddress() public {
    AddressInput.get(tvm.functionId(startChecks),
                     "Which contract do you want to work with?");
  }

  function startChecks(address value) public {
    Sdk.getAccountType(tvm.functionId(checkStatus), value);
    g_contract = value;
  }

  function checkStatus(int8 acc_type) public {
    if (!_checkActiveStatus(acc_type, "Contract")) {
      askContractAddress ();
    } else {
      Sdk.getAccountCodeHash(tvm.functionId(checkContractHash), g_contract);
    }
  }

  function _checkActiveStatus(int8 acc_type, string obj)
    private returns (bool) {
    if (acc_type == -1)  {
      Terminal.print(0, obj + " is inactive");
      return false;
    }
    if (acc_type == 0) {
      Terminal.print(0, obj + " is uninitialized");
      return false;
    }
    if (acc_type == 2) {
      Terminal.print(0, obj + " is frozen");
      return false;
    }
    return true;
  }

  function checkContractHash(uint256 code_hash) public {
    // compare with the expected code_hash
    if ( code_hash != 0x1111111111111111111111111111 ) {
      askContractAddress();
    } else {
      _gotoMainMenu();
    }
  }

  function _gotoMainMenu() public  {
    Sdk.getBalance(tvm.functionId(printMainMenu), g_contract );
  }

  function printMainMenu( uint128 nanotokens ) public {
    g_balance = nanotokens;
    string str = format("This wallet has {} tokens on the balance.",
                        tonsToStr(g_balance) );
    Terminal.print(0, str);

    MenuItem[] items;
    items.push( MenuItem("Change Contract", "",
                         tvm.functionId(askContractAddress)) );
    items.push( MenuItem("Call Getter", "",
                         tvm.functionId(callGetter)) );
    Menu.select("What's next?", "", items);
  }

  function callGetter() public {
    optional(uint256) pubkey = 0;
    g_retryId = tvm.functionId(callGetter);
    IContract(g_contract).getter{
        abiVer: 2,
        extMsg: true,
        sign: true,
        pubkey: pubkey,
        time: uint64(now),
        expire: 0,
        callbackId: tvm.functionId(onSuccess),
        onErrorId: tvm.functionId(onError)
        }() ;
  }

  function onError(uint32 sdkError, uint32 exitCode) public {
    exitCode = exitCode; sdkError = sdkError;
    ConfirmInput.get(g_retryId,
                     "Transaction failed. Do you want to retry transaction?");
  }

  function onSuccess(uint256 y) public {
    Terminal.print(0, format("Contract returned {}", y) );
    _gotoMainMenu () ;
  }
  
  //
  // Functions for external or internal invoke.
  //
  
  function other_entry(uint64 x) public pure returns (uint64 y) {
    y = x ;
  }
  

  //
  // Standard functions
  //
  
  function onCodeUpgrade() internal override {
    tvm.resetStorage();
  }
  
  function setIcon(bytes icon) public {
    require(msg.pubkey() == tvm.pubkey(), 100);
      tvm.accept();
      m_icon = icon;
  }
  
  function start() public override {
    askContractAddress ();
  }
  
  /// @notice Returns Metadata about DeBot.
  function getDebotInfo() public functionID(0xDEB)
    override view
    returns(
            string name, string version, string publisher, string caption,
            string author, address support, string hello, string language,
            string dabi, bytes icon
            ) {
    name = debot_name;
    version = format("{}.{}.{}",
                     debot_version_major,
                     debot_version_minor,
                     debot_version_fix);
    publisher = debot_publisher ;
    caption = debot_caption ;
    author = debot_author ;
    support = address.makeAddrStd(0, debot_support);
    hello = debot_hello ;
    language = debot_language ;
    dabi = m_debotAbi.get();
    icon = m_icon;
  }
  
  function getRequiredInterfaces() public view override
    returns (uint256[] interfaces) {
    return [
            Terminal.ID,
            AmountInput.ID,
            ConfirmInput.ID,
              AddressInput.ID,
            Menu.ID ];
  }
  
}
