package refactorerl.ui.core.types;

import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class RefactorerlPosrange extends OtpErlangTuple {

	private static final long serialVersionUID = 305983338703309239L;
	
	public RefactorerlPosrange(int[] args) {
		super(new OtpErlangObject[] {new OtpErlangInt(args[0]), new OtpErlangInt(args[1])});
	}

}
