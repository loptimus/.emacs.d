package refactorerl.ui.core.types;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class RefactorerlKeyValuePair extends OtpErlangTuple {
	
	private static final long serialVersionUID = 3656557551624933323L;

	public RefactorerlKeyValuePair(String name, OtpErlangObject value) {
		super(new OtpErlangObject[] { new OtpErlangAtom(name), value });
	}

}
