package refactorerl.ui.core.utils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
/**
* 
* @author  Gergely Horvath 
* 
*/

public class ProplistUtils {
	/**
	 * Get tuple with key from proplist. 
	 * @param key Property key.
	 * @param list Proplist
	 * @return Property value, or null if key not exist.
	 */
	public static OtpErlangObject getValue(String key, OtpErlangList list) {
		for (OtpErlangObject item : list) {
			if (item instanceof OtpErlangTuple) {
				OtpErlangTuple tuple = (OtpErlangTuple)item;
				
				if ((tuple.arity() >= 2) && tuple.elementAt(0).toString().equals(key)) {
					return tuple.elementAt(1);
				}
			}
		}
		return null;
	}
}
