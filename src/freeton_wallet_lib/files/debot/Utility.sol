
import "Terminal.sol";

abstract contract Utility {
  uint32 constant DAY_SECONDS = 86400 ;

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
  
  function _checkActiveStatus(int8 acc_type, string obj)
    internal returns (bool) {
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

  function _computeDelay( uint64 date ) internal pure returns (string)
  {
    if( date < now ){
      return "passed";
    } else {
      uint64 ndays ;
      uint64 nhours ;
      uint64 nmins ;
      uint64 nsecs ;
      ( ndays, date ) = math.muldivmod( date - now, 1, DAY_SECONDS );
      ( nhours, date ) = math.muldivmod( date, 1, 3600 );
      ( nmins, nsecs ) = math.muldivmod( date, 1, 60 );
      return format("in {} days {}h {}m {}s",
                    ndays, nhours, nmins, nsecs );
    }
  }

}
